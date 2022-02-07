
if [ -d "$HOME/esp/esp-idf" ] ; then
	export IDF_PATH="$HOME/esp/esp-idf"
	export PATH=$PATH:$IDF_PATH/tools
	. $IDF_PATH/export.sh
fi
