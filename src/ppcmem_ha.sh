#!/bin/sh

#echo 'Looking for modified files using svn diff...'
#FILES=`sh newfiles.sh`
#
# all the files that are *not* linked from mem.new or litmus (which have their own headers) or the lem-generated MachineDef* files

FILES="archExtra.ml arch.mli globals.ml  globals.mli  interact.ml  interact.mli iso.mli PPCArch.ml PPCIso.ml  pp.ml  pp.mli run.ml run.mli top.ml transitions.ml transitions.mli types.ml ui.mli version.ml"

echo 'Adding headers to the files...'
headache -c ppcmem_ha_cfg -h ppcmem_ha_hdr $FILES
echo 'Done.'
