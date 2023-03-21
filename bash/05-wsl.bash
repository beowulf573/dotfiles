# work around odd bug in wsl, would get Invalid Arguments otherwise
pushd /mnt/c
export WSL_VERSION=$(wsl.exe -l -v | grep -a '[*]' | sed 's/[^0-9]*//g')
if [ "$WSL_VERSION" -eq "2" ] ; then
	export WSL_HOST=$(tail -1 /etc/resolv.conf | cut -d' ' -f2)
	export DISPLAY=$WSL_HOST:0.0
else
	export DISPLAY=127.0.0.1:0.0
fi
popd
