(* LieLink automatic installer.*)

(* Load Rolf Mertig's package installer. *)
Import["https://packageinstaller.googlecode.com/hg/PackageInstaller/PackageInstaller.m"];

(* Install LieLink. *)
InstallPackage[
	"https://github.com/teake/LieLink/releases/download/0.4.1/LieLink.0.4.1.zip", 
	Directory -> FileNameJoin[{$UserBaseDirectory, "Applications"}], 
	Print -> False
]