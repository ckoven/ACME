#! /bin/csh -fv

if !(-d $OBJROOT/ocn/obj ) mkdir -p $OBJROOT/glc/obj || exit 2
if !(-d $OBJROOT/ocn/source) mkdir -p $OBJROOT/glc/source || exit 3
if !(-d $OBJROOT/ocn/input ) mkdir -p $OBJROOT/glc/input || exit 4

set my_path = $CASEROOT/SourceMods/src.mpas-li

echo -----------------------------------------------------------------
echo Copy the necessary files into $OBJROOT/glc/source
echo -----------------------------------------------------------------

cd $OBJROOT/glc/source

cp -fpR $CODEROOT/glc/mpas-li/model/src/* .
cp -fpR $CODEROOT/glc/mpas-li/driver glc_acme_driver

make all CORE=landice MODE=forward ESM=true DRIVER=glc_acme_driver || exit 5

## COPY ALL MODULE FILES TO THE OCEAN OBJ DIRECTORY ##
find . -name "*.mod" -exec cp -p {} $OBJROOT/glc/obj/. \;

## COPY LIBGLC TO LIBGLC IN LIBROOT ##
cp -p libglc.a ${LIBROOT}/libglc.a
