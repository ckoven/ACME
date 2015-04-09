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

if ( $?CRAY_CPU_TARGET ) then
	if ! ( "X$CRAY_CPU_TARGET" == "X" ) then
		set BACKUP_CRAY_CPU_TARGET = $CRAY_CPU_TARGET
		setenv CRAY_CPU_TARGET ""
	endif
endif

make build_tools ESM=ACME ROOT_DIR=`pwd`

if ( $?BACKUP_CRAY_CPU_TARGET ) then
	if ! ( "X$BACKUP_CRAY_CPU_TARGET" == "X" ) then
		setenv CRAY_CPU_TARGET $BACKUP_CRAY_CPU_TARGET
		unset BACKUP_CRAY_CPU_TARGET
	endif
endif

if ( `uname -s` == "AIX" ) then
	make all CORE=ocean MODE=forward ESM=ACME DRIVER=ocean_cesm_driver NO_TOOLS=true GEN_F90=true ROOT_DIR=`pwd` || exit 5
else
	make all CORE=ocean MODE=forward ESM=ACME DRIVER=ocean_cesm_driver NO_TOOLS=true ROOT_DIR=`pwd` || exit 5
endif

## COPY ALL MODULE FILES TO THE OCEAN OBJ DIRECTORY ##
find . -name "*.mod" -exec cp -p {} $OBJROOT/ocn/obj/. \;

## COPY LIBOCEAN TO LIBOCN IN LIBROOT ##
cp -p libocn.a ${LIBROOT}/libocn.a

