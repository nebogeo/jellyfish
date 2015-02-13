#
# Regular cron jobs for the jellyfish package
#
0 4	* * *	root	[ -x /usr/bin/jellyfish_maintenance ] && /usr/bin/jellyfish_maintenance
