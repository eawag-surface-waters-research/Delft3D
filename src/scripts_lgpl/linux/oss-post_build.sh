#!/bin/bash

if [ "$1" == '' ]; then
    echo "ERROR: No install_dir directory specified as argument of install.sh"
    exit 1
fi
install_dir=$1

if [ "$2" == '' ]; then
    echo "ERROR: No build directory specified as argument of install.sh"
    exit 1
fi
build_dir=$2

if [ "$3" == '' ]; then
    echo "ERROR: No checkout_src_root specified as argument of install.sh"
    exit 1
fi
checkout_src_root=$3

if [ "$4" == '' ]; then
    echo "ERROR: No configuration specified as argument of install.sh"
    exit 1
fi
configuration=$4

if [ "$5" == '' ]; then
    # Install all engines
    project = install_all
else
    project=$5
fi

echo "Script arguments "
echo "install_dir $install_dir"
echo "build_dir $build_dir"
echo "checkout_src_root $checkout_src_root"
echo "configuration $configuration"
echo "project $project"

install_folder="lnx64"
if [ $configuration == 'Debug' ]; then
   install_folder="lnx64_debug"
fi

# set directories
dest_bin="$install_dir/$install_folder/bin/"
dest_menu="$install_dir/$install_folder/menu"
dest_scripts="$install_dir/$install_folder/scripts"
dest_lib="$install_dir/$install_folder/lib"

# The following libraries must be removed from the list created by gatherScript:
# - system dependent libraries in the directories /lib and /lib64
# - libraries generated in the oss tree itself
gatherExcludeFilter="-e '^/lib/' -e '^/lib64/' -e 'flow2d3d' -e 'DelftOnline'"
gatherIncludeFilter="-e 'expat' -e 'libssl' -e 'libcrypto'"

# This script uses the command ldd to collect all dynamic libraries used:
gatherScript=$checkout_src_root/scripts_lgpl/linux/gatherlibraries.rb

# ===============================
# === copyFile: handles error ===
# ===============================
function copyFile () {
    # This function can handle wild characters in the arguments,
    # as long as they are quoted
    # example: copyFile "bin/*" targetdir

    # handle the error
    for file in $1
    do
        eval cp -fp $file $2
        if [ $? != 0 ]; then
            echo "can't copy \"$file\" to \"$2\"" 1>&2
        fi
    done

    return
}

# ===============================
# === copyFile: handles error ===
# ===============================
function make_directories() {

    mkdir -p $dest_bin
    mkdir -p $dest_lib
    mkdir -p $dest_menu
    mkdir -p $dest_scripts
    
}

function copy_binaries() {

    copyFile $binary_file $dest_bin
    
    echo "Gathering libraries for $binary_file ..."
    cp -u `$gatherScript $binary_file | eval grep -v $gatherExcludeFilter` $dest_bin
    cp -u `$gatherScript $binary_file | eval grep $gatherIncludeFilter`    $dest_bin
    
}


# =====================
# === POSTBUILD_ALL ===
# =====================
function install_all () {
    echo "installing all open source projects . . ."

    dflowfm
    dimr
    
    return
}

# ========================
# === POSTBUILD DFLOWFM ==
# ========================
function dflowfm () {

    echo "postbuild dflowfm . . ."

    make_directories
    binary_file="$build_dir/dflowfm_cli_exe/dflowfm-cli" 
    copy_binaries

    return
}

# ============================
# === POSTBUILD DFLOWFM_DLL ==
# ============================
function dflowfm_dll () {

    echo "postbuild dflowfm_dll . . ."

    make_directories
    binary_file="$build_dir/dflowfm_lib/libdflowfm.so" 
    copy_binaries

    return
}

# =======================
# === POSTBUILD DIMR ====
# =======================
function dimr () {

    echo "postbuild dimr . . ."
    
    make_directories    
    binary_file="$build_dir/dimr/dimr"
    copy_binaries

    return
}

# =========================
# === POSTBUILD DIMR_LIB ==
# =========================
function dimr_lib () {

    echo "postbuild dimr lib . . ."
    
    make_directories    
    binary_file="$build_dir/dimr_lib/libdimr.so"
    copy_binaries
    
    copyFile "$checkout_src_root/engines_gpl/d_hydro/scripts/create_config_xml.tcl"                    $dest_menu
    copyFile "$checkout_src_root/engines_gpl/dimr/scripts/generic/lnx64/*"                             $dest_scripts
    
    return
}

# =======================
# === INSTALL_ESMF ======
# =======================
function gatherESMF () {
    cp $srcroot/third_party_open/esmf/lnx64/bin/libesmf.so                                              $dest_lib
}

# =======================
# === INSTALL_SHARED ====
# =======================
function gatherDependencies () {
    
    echo "Gathering dependent libraries . . ."

    echo "Gathering libraries for lib/* ..."
    cp -u `$gatherScript $install_dir/lib/* | eval grep -v $gatherExcludeFilter`                       $dest_lib
    cp -u `$gatherScript $install_dir/lib/* | eval grep $gatherIncludeFilter`                          $dest_lib


    echo "Gathering libraries for bin/* ..."
    cp -u `$gatherScript $install_dir/bin/* | eval grep -v $gatherExcludeFilter`                       $dest_lib
    cp -u `$gatherScript $install_dir/bin/* | eval grep $gatherIncludeFilter`                          $dest_lib

    return
}

# post build project
$project

# gather all dependency
gatherESMF
gatherDependencies

# executable permission
cd $dest_bin
chmod a+x `find . -type f -exec file {} \; | grep executable | grep -v "\.svn" | cut -d ":" -f 1 | xargs`

cd $srcdir