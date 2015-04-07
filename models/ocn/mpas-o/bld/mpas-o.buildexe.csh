#! /bin/csh -fv

if !(-d $OBJROOT/ocn/obj   ) mkdir -p $OBJROOT/ocn/obj    || exit 2
if !(-d $OBJROOT/ocn/source) mkdir -p $OBJROOT/ocn/source || exit 3 
if !(-d $OBJROOT/ocn/input ) mkdir -p $OBJROOT/ocn/input  || exit 4

set my_path = $CASEROOT/SourceMods/src.mpas-o

echo -----------------------------------------------------------------
echo  Copy the necessary files into $OBJROOT/ocn/source
echo -----------------------------------------------------------------

cd $OBJROOT/ocn/source

cp -fpR $CODEROOT/ocn/mpas-o/model/src/* .
cp -fpR $CODEROOT/ocn/mpas-o/driver ocean_cesm_driver

if ! ( $CRAY_CPU_TARGET == "" ) then
	set BACKUP_CRAY_CPU_TARGET = $CRAY_CPU_TARGET
	unset CRAY_CPU_TARGET
endif

cd tools
make all || exit 5
cd ../

if ! ( $BACKUP_CRAY_CPU_TARGET == "" ) then
	set CRAY_CPU_TARGET = $BACKUP_CRAY_CPU_TARGET
	unset BACKUP_CRAY_CPU_TARGET
endif

if ( `uname -s` == "AIX" ) then
	make all CORE=ocean MODE=forward ESM=ACME DRIVER=ocean_cesm_driver CPP_DEF_FLAG=-WF,-D || exit 5
else
	make all CORE=ocean MODE=forward ESM=ACME DRIVER=ocean_cesm_driver || exit 5
endif

## COPY ALL MODULE FILES TO THE OCEAN OBJ DIRECTORY ##
find . -name "*.mod" -exec cp -p {} $OBJROOT/ocn/obj/. \;

## COPY LIBOCEAN TO LIBOCN IN LIBROOT ##
cp -p libocn.a ${LIBROOT}/libocn.a

