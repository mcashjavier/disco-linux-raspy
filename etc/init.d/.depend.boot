TARGETS = mountkernfs.sh fake-hwclock hostname.sh udev keyboard-setup mountdevsubfs.sh checkroot.sh checkfs.sh kmod checkroot-bootclean.sh mountall.sh mountall-bootclean.sh urandom procps udev-finish networking mountnfs.sh mountnfs-bootclean.sh kbd console-setup x11-common alsa-utils raspi-config bootmisc.sh plymouth-log
INTERACTIVE = udev keyboard-setup checkroot.sh checkfs.sh kbd console-setup
udev: mountkernfs.sh
keyboard-setup: mountkernfs.sh udev
mountdevsubfs.sh: mountkernfs.sh udev
checkroot.sh: fake-hwclock keyboard-setup mountdevsubfs.sh hostname.sh
checkfs.sh: checkroot.sh
kmod: checkroot.sh
checkroot-bootclean.sh: checkroot.sh
mountall.sh: checkfs.sh checkroot-bootclean.sh
mountall-bootclean.sh: mountall.sh
urandom: mountall.sh mountall-bootclean.sh
procps: mountkernfs.sh mountall.sh mountall-bootclean.sh udev
udev-finish: udev mountall.sh mountall-bootclean.sh
networking: mountkernfs.sh mountall.sh mountall-bootclean.sh urandom procps
mountnfs.sh: mountall.sh mountall-bootclean.sh networking
mountnfs-bootclean.sh: mountall.sh mountall-bootclean.sh mountnfs.sh
kbd: mountall.sh mountall-bootclean.sh mountnfs.sh mountnfs-bootclean.sh
console-setup: mountall.sh mountall-bootclean.sh mountnfs.sh mountnfs-bootclean.sh kbd
x11-common: mountall.sh mountall-bootclean.sh mountnfs.sh mountnfs-bootclean.sh
alsa-utils: mountall.sh mountall-bootclean.sh mountnfs.sh mountnfs-bootclean.sh
raspi-config: udev mountkernfs.sh mountall.sh mountall-bootclean.sh mountnfs.sh mountnfs-bootclean.sh
bootmisc.sh: mountall-bootclean.sh checkroot-bootclean.sh mountall.sh mountnfs.sh mountnfs-bootclean.sh udev
plymouth-log: mountall.sh mountall-bootclean.sh mountnfs.sh mountnfs-bootclean.sh
