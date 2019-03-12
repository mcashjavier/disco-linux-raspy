/*
 * Copyright © 2014 Collabora Ltd.
 *     @Author Marco Barisione <marco.barisione@collabora.co.uk>
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 2, or (at your option)
 * any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
 *
 * NOTE: Parts of this file (clearly marked so) are copied from youtube-dl
 * <https://github.com/rg3/youtube-dl>.
 * youtube-dl is released into the public domain, see <http://unlicense.org/>
 * and so it was relicensed under GPL to fit with the rest of the program.
 */

function parseQueryString(query) {
    if (query[0] == '?')
        query = query.substr(1);

    var params = {};
    var splitQuery = query.split('&');
    for (var i = 0; i < splitQuery.length; i++) {
        var separatorIndex = splitQuery[i].indexOf('=');
        if (separatorIndex == -1)
            continue;
        var key = splitQuery[i].slice(0, separatorIndex);
        var value = splitQuery[i].slice(separatorIndex + 1);
        if (!params[key])
            params[key] = [];
        params[key].push(unescape(value));
    }

    return params;
}

function EmbedLoader(videoId, delayLoad) {
    this.videoId = videoId; // The service-specific video ID
    this.video = null; // The <video> element
    this.container = null; // The <div> where to insert the video/error
    this.poster = null; // The <img> for the poster
    this.playArea = null; // The play button area
    this.delayLoad = delayLoad; // Whether to load the video now or later
    this.videoLoaded = false; // Whether the video is already loaded
}

EmbedLoader.prototype.sortFormats = function(formats, maxWidth, maxHeight) {
    var good = [];    // Fit in screen and generally useful.
    var bad = [];     // They don't fit but they are good videos.
    var veryBad = []; // Not very useful.
    var maxSmoothHeight = 720;

    for (var i = 0; i < formats.length; i++) {
        var format = formats[i];
        if (format.height === undefined)
            veryBad.push(format); // Not very useful without height.
        else if (format.preference < 0)
            veryBad.push(format); // Not a very good format according to youtube-dl.
        else if (format.containerFormatPreference < 0)
            veryBad.push(format); // Not a very good format for the Pi.
        else if (format.height > maxSmoothHeight)
            veryBad.push(format); // We cannot play this smoothly.
        else if (format.width === undefined)
            bad.push(format); // No width is still ok as height is what matters the most.
        else if (format.width > maxWidth || format.height > maxHeight)
            bad.push(format); // It doesn't fit, but it's ok to use.
        else
            good.push(format);
    }

    function compareGeneric(a, b, heightComparisonMultiplier) {
        // Bad ones (with lower preference) always go to the back.
        var cmp = b.preference - a.preference;
        if (cmp != 0)
            return cmp;

        // If both have the same preference then we look at height.
        // If heightComparisonMultiplier > 0 then the comparison is ascending.
        // If heightComparisonMultiplier < 0 then the comparison is descending.
        cmp = (a.height || 0) - (b.height || 0);
        if (cmp != 0)
            return cmp * heightComparisonMultiplier;

        // Same size and preference, so let's look at the best format.
        return b.containerFormatPreference - a.containerFormatPreference;
    }

    function compareSmallToBig(a, b) {
        return compareGeneric(a, b, 1);
    }

    function compareBigToSmall(a, b) {
        return compareGeneric(a, b, -1);
    }

    good.sort(compareBigToSmall);
    bad.sort(compareSmallToBig);
    veryBad.sort(compareSmallToBig);

    var sortedFormats = good.concat(bad).concat(veryBad);
    this.debugFormats(sortedFormats);
    return sortedFormats;
}

EmbedLoader.prototype.debugFormats = function(formats) {
    // Comment this return to debug the format choice.
    return;

    function representSize(size) {
        if (isNaN(size))
            return '???';
        else
            return size.toString();
    }

    for (var i = formats.length - 1; i >= 0; i--) {
        var f = formats[i];
        console.log(
                "== stream [" + i + "] = {" +
                "id: " + f.format_id + ", " +
                "ext: " + f.ext + ", " +
                "size: " + representSize(f.width) + "x" + representSize(f.height) + ", " +
                "note: " + f.format_note + ", " +
                "pref: " + f.preference + ", " +
                "url: " + f.url + "}");
    }

    console.log("The stream with format id " + formats[0].format_id +
            " (extension: " + formats[0].ext + ", size: " +
            representSize(formats[0].width) + "x" + representSize(formats[0].height) +
            ") will be played in a " +
            this.container.offsetWidth + "x" + this.container.offsetHeight +
            " container, from " + formats[0].url);
}

EmbedLoader.prototype.displayError = function (errorMessage) {
    console.log("error: EmbedLoader: The video id:" + this.videoId + " could not be loaded: " + errorMessage);
    if (this.video)
        this.video.parentNode.removeChild(this.video);

    if (this.videoId)
        document.getElementById('error-video-id').textContent = this.videoId;

    if (errorMessage)
        document.getElementById('error-message').textContent = errorMessage;

    document.getElementById('error').style.display = 'initial';
}

EmbedLoader.prototype.loadVideo = function(callback) {
    if (this.videoLoaded) {
        callback(true);
        return;
    }
    this.videoLoaded = true;

    this.getFormats(function(parsedResults) {
        var success = parsedResults.success();
        if (success) {
            formats = this.sortFormats(parsedResults.formats,
                    this.container.offsetWidth, this.container.offsetHeight);
            this.video.setAttribute('src', formats[0].url);
            this.video.setAttribute('controls', '');
        } else {
            this.displayError(parsedResults.error);
        }
        callback(success);
    }.bind(this));
}

EmbedLoader.prototype.startVideo = function() {
    this.loadVideo(function(success){
        if (!success)
            return;
        this.video.style.display = 'initial';
        // Setting autoplay at this stage doesn't work.
        setTimeout(function() { this.video.play(); }.bind(this), 1000);
        this.poster.style.transition = 'opacity .5s';
        this.poster.style.opacity = 0;
        this.playArea.style.transition = 'opacity .5s';
        this.playArea.style.opacity = 0;
        setTimeout(function() {
            this.poster.parentNode.removeChild(this.poster);
            this.playArea.parentNode.removeChild(this.playArea);
        }.bind(this), 2200);
    }.bind(this));
}

EmbedLoader.prototype.playButtonClass = function() {
    return 'blue';
}

EmbedLoader.prototype.createPlayButton = function() {
    var playButton = document.getElementById('play-button');
    playButton.className = this.playButtonClass();

    this.playArea = document.getElementById('play-area');
    this.playArea.onclick = this.startVideo.bind(this);
}

EmbedLoader.prototype.posterImagesHaveHeight = function() {
    // Services that don't have height in the thumbnail data can override
    // this.
    return true;
}

EmbedLoader.prototype.debugPosterImages = function(images) {
    // Comment this return to debug the poster choice.
    return;

    function representSize(size) {
        if (size == null || isNaN(size))
            return '???';
        else
            return size.toString();
    }

    for (var i = images.length - 1; i >= 0; i--) {
        var img = images[i];
        console.log(
                "== poster[" + i + "] = { " +
                "size: " + representSize(img.width) + "x" + representSize(img.height) + ", " +
                "url: " + img.url + "}");
    }

    console.log("The image with size " + representSize(images[0].width) + "x" + representSize(images[0].height) +
            " will be used, from " + images[0].url);
}

EmbedLoader.prototype.getPreferredPosterImage = function(width, height, callback) {
    this.getPosterImages(function(allImages) {
        var smallImages = [];
        var bigImages = [];
        var badSizeImages = [];

        for (var i = 0; i < allImages.length; i++) {
            var img = allImages[i];

            if (!img.url)
                continue;

            if (!img.width) {
                badSizeImages.push(img);
                continue;
            }

            // Some services just don't give us any width, so we cannot just
            // throw away all of their thumbnails.
            if (!img.height && this.posterImagesHaveHeight()) {
                badSizeImages.push(img);
                continue;
            }

            if (img.width > width)
                bigImages.push(img);
            if (img.height && img.height > height)
                bigImages.push(img);
            else
                smallImages.push(img);
        }

        function compareSmallToBig(a, b) {
            return a.height - b.height;
        }

        function compareBigToSmall(a, b) {
            return -compareSmallToBig(a, b);
        }

        bigImages.sort(compareSmallToBig);
        smallImages.sort(compareBigToSmall);
        var sortedImages = [].concat(bigImages, smallImages, badSizeImages);

        this.debugPosterImages(sortedImages);

        if (sortedImages.length)
            callback(sortedImages[0]);
        else
            callback(null);
    }.bind(this));
}

EmbedLoader.prototype.createPosterNow = function(image) {
    this.poster = document.getElementById('poster');
    this.poster.style.backgroundImage = 'url('+image.url+')';
    setTimeout(function() {
        this.poster.style.opacity = 1;
    }.bind(this), 250);
}

EmbedLoader.prototype.createPoster = function() {
    var containerWidth = this.container.offsetWidth;
    var containerHeight = this.container.offsetHeight;
    this.getPreferredPosterImage(containerWidth, containerHeight, function(image) {
        if (!image)
            return;

        this.createPosterNow(image);
    }.bind(this));
}

EmbedLoader.prototype.createVideoTag = function() {
    this.video = document.getElementById('yt-replacement-video');
    // Initially hidden as we have the poster.
    this.video.style.display = 'none';
}

EmbedLoader.prototype.insertVideo = function(container) {
    this.container = container;

    this.createVideoTag();
    this.createPoster();
    this.createPlayButton();
    if (!this.delayLoad)
        this.loadVideo(function(){});
}


// Handle failed service detection

EmbedLoaderFailure.prototype = Object.create(EmbedLoader.prototype);
EmbedLoaderFailure.prototype.constructor = EmbedLoaderFailure;

function EmbedLoaderFailure(errorMessage) {
    this.errorMessage = errorMessage;
}

EmbedLoaderFailure.prototype.insertVideo = function(container) {
    this.displayError(this.errorMessage);
}


// YouTube

EmbedLoaderYouTube.prototype = Object.create(EmbedLoader.prototype);
EmbedLoaderYouTube.prototype.constructor = EmbedLoaderYouTube;

function EmbedLoaderYouTube(videoId, delayLoad) {
    this.dataFromYouTube = null;

    EmbedLoader.call(this, videoId, delayLoad);
}

EmbedLoaderYouTube.prototype.getPosterImages = function(callback) {
    var maxres = "https://img.youtube.com/vi/" + this.videoId  + "/maxresdefault.jpg";
    _http_head(maxres, function(response){
        var posters = [
            { width: 120, height:  90, url: "https://img.youtube.com/vi/" + this.videoId + "/default.jpg" },
            { width: 320, height: 180, url: "https://img.youtube.com/vi/" + this.videoId  + "/mqdefault.jpg" },
            { width: 480, height: 360, url: "https://img.youtube.com/vi/" + this.videoId  + "/hqdefault.jpg" },
            { width: 640, height: 480, url: "https://img.youtube.com/vi/" + this.videoId  + "/sddefault.jpg" },
        ];
        if (response.status == 200)
            posters.push({ width: 641, height: 481, url: maxres }); // just make sure this is bigger than the above
        callback(posters);
    }.bind(this));
}

EmbedLoaderYouTube.prototype.playButtonClass = function() {
    return 'red';
}

EmbedLoaderYouTube.prototype.getFormats = function(callback) {
    if (this.dataFromYouTube) {
        callback(this.dataFromYouTube);
        return;
    }

    youtubeGetFormatsFromId(this.videoId, function(dataFromYouTube) {
        this.dataFromYouTube = dataFromYouTube;
        callback(this.dataFromYouTube);
    }.bind(this));
}


// Vimeo

EmbedLoaderVimeo.prototype = Object.create(EmbedLoader.prototype);
EmbedLoaderVimeo.prototype.constructor = EmbedLoaderVimeo;

function EmbedLoaderVimeo(videoId, delayLoad) {
    this.dataFromVimeo = null;

    EmbedLoader.call(this, videoId, delayLoad);
}

EmbedLoaderVimeo.prototype.ensureDataFromVimeo = function(callback) {
    if (this.dataFromVimeo) {
        callback(this.dataFromVimeo);
        return;
    }

    vimeoGetFormatsAndThumbsFromId(this.videoId, function(dataFromVimeo) {
        this.dataFromVimeo = dataFromVimeo;
        callback(this.dataFromVimeo);
    }.bind(this));
}

EmbedLoaderVimeo.prototype.getPosterImages = function(callback) {
    this.ensureDataFromVimeo(function() {
        if (this.dataFromVimeo.success())
            callback(this.dataFromVimeo.thumbnails);
        else
            callback([]);
    }.bind(this));
}

EmbedLoaderVimeo.prototype.posterImagesHaveHeight = function() {
    return false;
}

EmbedLoaderVimeo.prototype.getFormats = function(callback) {
    this.ensureDataFromVimeo(function() {
        callback(this.dataFromVimeo);
    }.bind(this));
}


// EmbedLoader creation

function createEmbedLoaderForUrl(url, delayLoad) {
    if (!url)
        return new EmbedLoaderFailure("no video specified");

    matchInfo = url.match(/youtube\.com\/embed\/([^?]*)/);
    if (matchInfo)
        return new EmbedLoaderYouTube(matchInfo[1], delayLoad);

    matchInfo = url.match(/\/player\.vimeo\.com\/video\/([^?]*)/);
    if (matchInfo)
        return new EmbedLoaderVimeo(matchInfo[1], delayLoad);

    return new EmbedLoaderFailure(url + " is not a supported embed URL");
}


// The following code is based on the youtube-dl program from
// <https://github.com/rg3/youtube-dl> and translated from Python
// to JavaScript. This translation was done in a way to minimise
// unneeded change, so it will be easier to update the code in the
// future. This explains the weird non-idiomatic coding style.
//
// youtube-dl's original code was released into the public domain,
// see <http://unlicense.org/>.
//
// The latest commit in the youtube-dl's version this is based on is:
// commit 29f6ed78e87946979ab6472b118a4da7cf7ef0c0
// Author: Petr Půlpán <Pulpan3@gmail.com>
// Date:   Tue Jul 1 10:35:49 2014 +0200

var _formats = {
    '5': {'ext': 'flv', 'width': 400, 'height': 240},
    '6': {'ext': 'flv', 'width': 450, 'height': 270},
    '13': {'ext': '3gp'},
    '17': {'ext': '3gp', 'width': 176, 'height': 144},
    '18': {'ext': 'mp4', 'width': 640, 'height': 360},
    '22': {'ext': 'mp4', 'width': 1280, 'height': 720},
    '34': {'ext': 'flv', 'width': 640, 'height': 360},
    '35': {'ext': 'flv', 'width': 854, 'height': 480},
    '36': {'ext': '3gp', 'width': 320, 'height': 240},
    '37': {'ext': 'mp4', 'width': 1920, 'height': 1080},
    '38': {'ext': 'mp4', 'width': 4096, 'height': 3072},
    '43': {'ext': 'webm', 'width': 640, 'height': 360},
    '44': {'ext': 'webm', 'width': 854, 'height': 480},
    '45': {'ext': 'webm', 'width': 1280, 'height': 720},
    '46': {'ext': 'webm', 'width': 1920, 'height': 1080},

    // 3d videos
    '82': {'ext': 'mp4', 'height': 360, 'format_note': '3D', 'preference': -20},
    '83': {'ext': 'mp4', 'height': 480, 'format_note': '3D', 'preference': -20},
    '84': {'ext': 'mp4', 'height': 720, 'format_note': '3D', 'preference': -20},
    '85': {'ext': 'mp4', 'height': 1080, 'format_note': '3D', 'preference': -20},
    '100': {'ext': 'webm', 'height': 360, 'format_note': '3D', 'preference': -20},
    '101': {'ext': 'webm', 'height': 480, 'format_note': '3D', 'preference': -20},
    '102': {'ext': 'webm', 'height': 720, 'format_note': '3D', 'preference': -20},

    // Apple HTTP Live Streaming
    '92': {'ext': 'mp4', 'height': 240, 'format_note': 'HLS', 'preference': -10},
    '93': {'ext': 'mp4', 'height': 360, 'format_note': 'HLS', 'preference': -10},
    '94': {'ext': 'mp4', 'height': 480, 'format_note': 'HLS', 'preference': -10},
    '95': {'ext': 'mp4', 'height': 720, 'format_note': 'HLS', 'preference': -10},
    '96': {'ext': 'mp4', 'height': 1080, 'format_note': 'HLS', 'preference': -10},
    '132': {'ext': 'mp4', 'height': 240, 'format_note': 'HLS', 'preference': -10},
    '151': {'ext': 'mp4', 'height': 72, 'format_note': 'HLS', 'preference': -10},

    // DASH mp4 video
    '133': {'ext': 'mp4', 'height': 240, 'format_note': 'DASH video', 'acodec': 'none', 'preference': -40},
    '134': {'ext': 'mp4', 'height': 360, 'format_note': 'DASH video', 'acodec': 'none', 'preference': -40},
    '135': {'ext': 'mp4', 'height': 480, 'format_note': 'DASH video', 'acodec': 'none', 'preference': -40},
    '136': {'ext': 'mp4', 'height': 720, 'format_note': 'DASH video', 'acodec': 'none', 'preference': -40},
    '137': {'ext': 'mp4', 'height': 1080, 'format_note': 'DASH video', 'acodec': 'none', 'preference': -40},
    '138': {'ext': 'mp4', 'height': 2160, 'format_note': 'DASH video', 'acodec': 'none', 'preference': -40},
    '160': {'ext': 'mp4', 'height': 144, 'format_note': 'DASH video', 'acodec': 'none', 'preference': -40},
    '264': {'ext': 'mp4', 'height': 1440, 'format_note': 'DASH video', 'acodec': 'none', 'preference': -40},

    // Dash mp4 audio
    '139': {'ext': 'm4a', 'format_note': 'DASH audio', 'vcodec': 'none', 'abr': 48, 'preference': -50},
    '140': {'ext': 'm4a', 'format_note': 'DASH audio', 'vcodec': 'none', 'abr': 128, 'preference': -50},
    '141': {'ext': 'm4a', 'format_note': 'DASH audio', 'vcodec': 'none', 'abr': 256, 'preference': -50},

    // Dash webm
    '167': {'ext': 'webm', 'height': 360, 'width': 640, 'format_note': 'DASH video', 'acodec': 'none', 'container': 'webm', 'vcodec': 'VP8', 'preference': -40},
    '168': {'ext': 'webm', 'height': 480, 'width': 854, 'format_note': 'DASH video', 'acodec': 'none', 'container': 'webm', 'vcodec': 'VP8', 'preference': -40},
    '169': {'ext': 'webm', 'height': 720, 'width': 1280, 'format_note': 'DASH video', 'acodec': 'none', 'container': 'webm', 'vcodec': 'VP8', 'preference': -40},
    '170': {'ext': 'webm', 'height': 1080, 'width': 1920, 'format_note': 'DASH video', 'acodec': 'none', 'container': 'webm', 'vcodec': 'VP8', 'preference': -40},
    '218': {'ext': 'webm', 'height': 480, 'width': 854, 'format_note': 'DASH video', 'acodec': 'none', 'container': 'webm', 'vcodec': 'VP8', 'preference': -40},
    '219': {'ext': 'webm', 'height': 480, 'width': 854, 'format_note': 'DASH video', 'acodec': 'none', 'container': 'webm', 'vcodec': 'VP8', 'preference': -40},
    '242': {'ext': 'webm', 'height': 240, 'format_note': 'DASH video', 'acodec': 'none', 'preference': -40},
    '243': {'ext': 'webm', 'height': 360, 'format_note': 'DASH video', 'acodec': 'none', 'preference': -40},
    '244': {'ext': 'webm', 'height': 480, 'format_note': 'DASH video', 'acodec': 'none', 'preference': -40},
    '245': {'ext': 'webm', 'height': 480, 'format_note': 'DASH video', 'acodec': 'none', 'preference': -40},
    '246': {'ext': 'webm', 'height': 480, 'format_note': 'DASH video', 'acodec': 'none', 'preference': -40},
    '247': {'ext': 'webm', 'height': 720, 'format_note': 'DASH video', 'acodec': 'none', 'preference': -40},
    '248': {'ext': 'webm', 'height': 1080, 'format_note': 'DASH video', 'acodec': 'none', 'preference': -40},
    '271': {'ext': 'webm', 'height': 1440, 'format_note': 'DASH video', 'acodec': 'none', 'preference': -40},
    '272': {'ext': 'webm', 'height': 2160, 'format_note': 'DASH video', 'acodec': 'none', 'preference': -40},

    // Dash webm audio
    '171': {'ext': 'webm', 'vcodec': 'none', 'format_note': 'DASH audio', 'abr': 48, 'preference': -50},
    '172': {'ext': 'webm', 'vcodec': 'none', 'format_note': 'DASH audio', 'abr': 256, 'preference': -50},

    // RTMP (unnamed)
    '_rtmp': {'protocol': 'rtmp'},
};

// Compatibility with Python
String.prototype.startsWith = function(str) {
    return this.indexOf(str) == 0;
}

function _http_request(method, url, callback) {
    try {
        var request = new XMLHttpRequest();
        request.open(method, url, true);
        request.onload = function(){
            callback(this);
        };
        request.onerror = function() {
            console.log ("Failed to retrieve " + url);
            callback(null);
        };
        request.send();
    } catch (e) {
        console.log ("Failed to retrieve " + url);
        callback(null);
    }
}

function _http_head(url, callback) {
    _http_request('HEAD', url, callback);
}

function _download_webpage(url, callback) {
    _http_request('GET', url, function(response) {
        callback(response.responseText);
    });
}

compat_parse_qs = parseQueryString;

function uppercase_escape(string) {
    // This is used to fix the unicode in descriptions. We ignore description
    // so we just need to get rid of the \U escape sequences that would break
    // the JSON parser.
    return string.replace(/\\U[0-9a-fA-F]{8}/, '');
}

function build_result(formats, thumbnails, errMsg) {
    return { 'formats':    formats,
             'thumbnails': thumbnails,
             'error':      errMsg,
             success:      function() { return this.error == null; }};
}

function err(errMsg) {
    return build_result([], [], errMsg);
}

function success(formats, thumbnails) {
    if (!thumbnails)
        thumbnails = [];

    if (formats.length == 0)
        // Well, not really a success.
        return err("no available formats found");
    else
        return build_result(formats, thumbnails, null);
}

// This is the main extraction function and is based on YoutubeIE._real_extract
// from youtube_dl/extractor/youtube.py.
function youtubeGetFormatsFromId(video_id, callback) {
    proto = 'https';

    // Get video webpage
    url = proto + '://www.youtube.com/watch?v=' + video_id + '&gl=US&hl=en&has_verified=1';
    _download_webpage(url, function(video_webpage) {
        if (!video_webpage) {
            callback(err("Failed to retrieve " + url));
            return;
        }

        // Get video info
        if (video_webpage.match(/player-age-gate-content">/)) {
            // In youtube-dl the program gets the age confirmation page and it
            // needs to get the real page from it. In the browser we reach this
            // point only if the user is already logged in, so no need to bother
            // with age consent support.
            callback(err("age consent should not be reached at this stage"));
            return;
        }

        function yt_get_video_info(video_id, video_info_callback) {
            var els = ['&el=embedded', '&el=detailpage', '&el=vevo', ''];
            var progress = {
                left: els.length,
                found: false
            };
            for (var i = 0; i < els.length; i++) {
                el_type = els[i];
                var video_info_url = 'https://www.youtube.com/get_video_info?&video_id=' +
                    video_id + el_type + '&ps=default&eurl=&gl=US&hl=en';
                _download_webpage(video_info_url, function(video_info_webpage) {
                    if (progress.found)
                        return;

                    var video_info = compat_parse_qs(video_info_webpage);
                    if (video_info['token']) {
                        progress.found = true;
                        video_info_callback(video_info);
                    }

                    progress.left--;
                    if (progress.left == 0 && !progress.found)
                        video_info_callback(null);
                }.bind(this));
            }
        }

        function yt_process_video_info (video_info) {
            if (!video_info || !video_info['token']) {
                callback(err("cannot find the token"));
                return;
            }

            // Decide which formats to download
            var mobj = video_webpage.match(/;ytplayer\.config\s*=\s*({.*?});/);
            if (mobj) {
                json_code = uppercase_escape(mobj[1]);
                ytplayer_config = JSON.parse(json_code);
                args = ytplayer_config['args'];
                // Easy way to know if the 's' value is in url_encoded_fmt_stream_map
                // this signatures are encrypted
                if (args['url_encoded_fmt_stream_map']) {
                    re_signature = /[&,]s=/
                    m_s = args['url_encoded_fmt_stream_map'].search(re_signature);
                    if (m_s)
                        video_info['url_encoded_fmt_stream_map'] = [args['url_encoded_fmt_stream_map']];
                    adaptive_fmts = args['adaptive_fmts'];
                    if (!adaptive_fmts)
                        adaptive_fmts = '';
                    m_s = adaptive_fmts.match(re_signature);
                    if (m_s) {
                        if (video_info['adaptive_fmts'])
                            video_info['adaptive_fmts'][0] += ',' + args['adaptive_fmts'];
                        else
                            video_info['adaptive_fmts'] = [args['adaptive_fmts']];
                    }
                }
            }

            containerFormatPreferenceMap = {
                'mp4':  10,
                'webm':  9,
                '3gp':  -1,  // Not very good support but they can play
                'flv':  -10, // We cannot accelerate these
                'mpa':  -20, // Audio only
            };
            function _map_to_format_list(urlmap) {
                var formats = [];
                for (var itag in urlmap) {
                    if (!urlmap.hasOwnProperty(itag))
                        continue;
                    video_real_url = urlmap[itag];
                    dct = {
                        'format_id': itag,
                        'url': video_real_url.replace(/^http:/, 'https:')
                    };
                    var extra_info = _formats[itag];
                    if (extra_info) {
                        for(var k in extra_info) {
                            if(!extra_info.hasOwnProperty(k))
                                continue;
                            dct[k] = extra_info[k];
                        }
                    }
                    if (dct.preference === undefined)
                        dct.preference = 0;
                    dct.containerFormatPreference = containerFormatPreferenceMap[dct.ext] || 0;
                    formats.push(dct);
                }
                return formats;
            }

            url_encoded_fmt_stream_map = video_info['url_encoded_fmt_stream_map'] || [];
            adaptive_fmts = video_info['adaptive_fmts'] || [];

            var formats;
            if (video_info['conn'] && video_info['conn'][0].startsWith('rtmp')) {
                formats = [{
                    'format_id': '_rtmp',
                    'protocol': 'rtmp',
                    'url': video_info['conn'][0],
                }];
            } else if (url_encoded_fmt_stream_map.length >= 1 || adaptive_fmts.length >= 1) {
                if (!url_encoded_fmt_stream_map.length)
                    url_encoded_fmt_stream_map = [''];
                if (!adaptive_fmts.length)
                    adaptive_fmts = [''];
                encoded_url_map = url_encoded_fmt_stream_map[0] + ',' + adaptive_fmts[0];
                if (encoded_url_map.indexOf('rtmpe%3Dyes') != -1) {
                    callback(err("rtmpe downloads are not supported"));
                    return;
                }
                url_map = {};
                split_encoded_url_map = encoded_url_map.split(',');
                for (var i = 0; i < split_encoded_url_map.length; i++) {
                    url_data_str = split_encoded_url_map[i];
                    url_data = compat_parse_qs(url_data_str);
                    if (url_data['itag'] && url_data['url']) {
                        url = url_data['url'][0];
                        if (url_data['sig']) {
                            url += '&signature=' + url_data['sig'][0];
                        } else if (url_data['s']) {
                            return err("encrypted streams not supported");
                        }
                        if (url.indexOf('ratebypass') == -1) {
                            url += '&ratebypass=yes';
                        }
                        url_map[url_data['itag'][0]] = url;
                    }
                }
                formats = _map_to_format_list(url_map);
            } else if (video_info['hlsvp']) {
                callback(err("hlsvp videos are not supported"));
                return;
            } else {
                callback(err("no conn, hlsvp or url_encoded_fmt_stream_map information found in video info"));
                return;
            }

            callback(success(formats));
            return;
        }

        yt_get_video_info(video_id, yt_process_video_info);
    }.bind(this));
}

// This is the main extraction function and is based on VimeoIE._real_extract
// from youtube_dl/extractor/vimeo.py.
function vimeoGetFormatsAndThumbsFromId(video_id, callback) {
    var url = 'https://player.vimeo.com/video/' + video_id + '?rpi-allow=1';
    _download_webpage(url, function(webpage) {
        function vimeo_get_config(get_config_callback) {
            // Extract the config JSON
            var config_url = webpage.match(/ data-config-url="(.+?)"/);
            if (config_url && config_url[1]) {
                _download_webpage(config_url, function(config_json) {
                    get_config_callback(config_json);
                }.bind(this));
            } else {
                var config_json = null;
                // For pro videos or player.vimeo.com urls
                // We try to find out to which variable is assigned the config dic
                m_variable_name = webpage.match(/(\w)\.video\.id/);
                if (m_variable_name)
                    config_re = [ new RegExp(m_variable_name[1] + '=({.+?});') ];
                else
                    config_re = [ / = {config:({.+?}),assets:/, /(?:[abc])=({.+?});/ ];
                webpage = webpage.replace('\n', ''); // Not re.DOTALL in JavaScript...
                for (var i = 0; i < config_re.length; i++) {
                    mo = webpage.match(config_re[i]);
                    if (mo) {
                        config_json = mo[0];
                        break;
                    }
                }
                get_config_callback(config_json);
            }
        }

        vimeo_get_config(function(config_json) {
            if (!config_json) {
                callback(err("cannot retrieve the video configuration"));
                return;
            }
            // The returned JSON doesn't seem to be really JSON as there's an initial
            // variable assignment, like “a={... actual JSON ...}”.
            // We strip this so that JSON.parse() is happy.
            config_json = config_json.replace(/^[a-zA-Z]*\s*=\s*/, '');
            var config = JSON.parse(config_json);

            if (!config.video) {
                callback(err("unexpected JSON returned by the server"));
                return;
            }

            var video_thumbnails = [];
            if (config.video.thumbnail)
                video_thumbnails.push({ 'url': config.video.thumbnail, 'width': null, 'height': null });

            if (video_thumbnails.length == 0) {
                json_thumbs = config.video.thumbs;
                if (json_thumbs) {
                    for (var size in json_thumbs) {
                        if (!json_thumbs.hasOwnProperty(size))
                            continue;
                        if (isNaN (size))
                            sizeStored = null;
                        else
                            sizeStored = size;
                        video_thumbnails.push({ 'url': json_thumbs[size], 'width': sizeStored, 'height': null });
                    }
                }
            }

            // Vimeo specific: extract video codec and quality information
            // First consider quality, then codecs, then take everything
            var codecs = [['vp6', 'flv'], ['vp8', 'flv'], ['h264', 'mp4']];
            var files = {'hd': [], 'sd': [], 'other': []};
            var config_files = config.video.files;
            if (!config_files && config.request)
                config_files = config.request.files;
            if (!config_files) {
                callback(err("no video stream found"));
                return;
            }
            for (var i = 0; i < codecs.length; i++) {
                var codec_name = codecs[i][0];
                var codec_extension = codecs[i][1];
                var qualities = config_files[codec_name];
                if (!qualities)
                    qualities = {};
                for (var quality in qualities) {
                    if (!qualities.hasOwnProperty(quality))
                        continue;
                    var format_id = codec_name + "-" + quality.toLowerCase();
                    var key = files[quality];
                    if (!key || key == "")
                        key = "other";
                    var video_url = null;
                    var file_info;
                    if (config_files[codec_name]) {
                        file_info = config_files[codec_name][quality];
                        if (file_info)
                            video_url = file_info['url'];
                    } else {
                        file_info = {};
                    }
                    if (!video_url) {
                        video_url = "https://player.vimeo.com/play_redirect?" +
                            "clip_id=" + video_id +
                            "&sig=" + sig +
                            "&time=" + timestamp +
                            "&quality=" + quality +
                            "&codecs=" + codec_name.toUpperCase() +
                            "&type=moogaloop_local&embed_location=";
                    }

                    files[key].push({
                        'ext': codec_extension,
                        'url': video_url,
                        'format_id': format_id,
                        'width': file_info.width,
                        'height': file_info.height,
                    });
                }
            }
            var formats = [].concat(files.other, files.sd, files.hd);
            if (formats.length == 0) {
                callback(err('no known codec found'));
                return;
            }

            callback(success(formats, video_thumbnails));
        }.bind(this));
    }.bind(this));
}
