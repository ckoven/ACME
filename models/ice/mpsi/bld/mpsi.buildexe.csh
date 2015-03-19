#! /bin/csh -fv

if !(-d $OBJROOT/ice/obj   ) mkdir -p $OBJROOT/ice/obj    || exit 2
if !(-d $OBJROOT/ice/source) mkdir -p $OBJROOT/ice/source || exit 3 
if !(-d $OBJROOT/ice/input ) mkdir -p $OBJROOT/ice/input  || exit 4

#set my_path = $CASEROOT/SourceMods/src.mpas-o

echo -----------------------------------------------------------------
echo  Copy the necessary files into $OBJROOT/ocn/source
echo -----------------------------------------------------------------

cd $OBJROOT/ice/source

cp -fpR $CODEROOT/ice/mpsi/model/src/* .
cp -fpR $CODEROOT/ice/mpsi/driver seaice_cesm_driver

make CORE=cice ESM=true DRIVER=seaice_cesm_driver || exit 5

## COPY ALL MODULE FILES TO THE OCEAN OBJ DIRECTORY ##
find . -name "*.mod" -exec cp -p {} $OBJROOT/ice/obj/. \;

## COPY LIBOCEAN TO LIBOCN IN LIBROOT ##
cp -p libice.a ${LIBROOT}/libice.a

