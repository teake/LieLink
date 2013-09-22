(* LieLink automatic installer.*)

(* Check for MMA version. *)
General::mmaversion = "LieLink is incompatible with Mathematica `1`.";
If[System`$VersionNumber < 7.,
	Message[General::mmaversion, IntegerPart @ System`$VersionNumber];
	Abort[]
];

(* Load Rolf Mertig's package installer. *)
Import["https://packageinstaller.googlecode.com/hg/PackageInstaller/PackageInstaller.m"];

BeginPackage["LieLinkInstaller`"]

Begin["`Private`"]

(*************************************
 *                                   *
 *        Initialize variables       *
 *                                   *
 *************************************)

installVersion = "0.4.2";
zipURL = 
	StringJoin[
		"https://github.com/teake/LieLink/releases/download/",
		installVersion,
		"/LieLink.",
		installVersion,
		".zip"
	];

previousDir = FindFile["LieLink`"];

If[ previousDir =!= $Failed,
	installDir = FileNameJoin @ Drop[FileNameSplit@FindFile["LieLink`"], -3],
	installDir = FileNameJoin @ { $UserBaseDirectory, "Applications" }
];


(*************************************
 *                                   *
 *         Install functions         *
 *                                   *
 *************************************)

(* Present a popup-dialog before proceeding to install. *)
InstallLieLink[directory_String] := 
	Switch[
		InstallDialog @ directory
		,
		"OK",
		DoInstall @ directory
		,
		"ChangeDir",
		InstallLieLink @ SystemDialogInput[
				"Directory", 
				directory, 
				WindowTitle -> "Choose base directory to install LieLink into"
			]
		,
		"Cancel",
		$Canceled
		,
		_,
		Null
	];

(* Does the actual installation. *)
DoInstall[directory_] := 
	Module[
		{
			result
		},
		Print["Installing LieLink "<> installVersion <> " ..."];
		RenameItem @ FileNameJoin @ {directory, "LieLink"};
		result = PackageInstaller`InstallPackage[
			zipURL,
			Directory -> directory,
			Print->False
		];
		If[ result === directory,
			Print["Installation successful. Type \"<<LieLink`\" to load the package."],
			Print["Installation unsuccessful."]
		]
	];


(* Generates the install dialog *)
InstallDialog[directory_String] :=
	ChoiceDialog[
		StringJoin @
			{
				"LieLink " <> installVersion,
				" will be installed into the " <> InstallDirectoryString @ directory <> ".\n",
				Sequence @@ If[ !MemberQ[$Path, FileNameJoin @ FileNameSplit @ directory],
					"WARNING: this directory is not a member of $Path.\n",
					{}
				],
				"Any existing versions will be backed up.\n",
				"\nDo you want to continue?"
			}, 
		{ "OK" -> "OK", "Change directory" -> "ChangeDir", "Cancel" -> "Cancel" },
		WindowTitle -> "LieLink installer"
	] 

(* Changes the directory to a nice string if possible. *)
InstallDirectoryString[directory_String] :=
	Switch[
		FileNameJoin @ FileNameSplit @ directory
		,
		FileNameJoin @ { $UserBaseDirectory, "Applications" },
		"user directory for packages"
		,
		FileNameJoin @ { $BaseDirectory, "Applications" },
		"system directory for packages"
		,
		_,
		"directory " <> directory
	];

(* Renames a file or folder "file.ext" to "file.ext_old", 
   or if that's taken, "file.ext_old1" etc. *)
RenameItem[file_?FileExistsQ, renameextension_String:"_old"] := 
	Module[
		{
			newfile, existing, basenew
		},
		basenew		= FileNameTake[file] <> renameextension;
		existing 	= FileNameTake /@ FileNames[ basenew ~~ NumberString..., {ParentDirectory @ file} ];
		newfile		= basenew <> If[
			Length @ existing == 0,
			"", 
			ToString[ 1 + ( ToExpression[ StringDrop[ Last @ existing, StringLength @ basenew ] ] /. Null -> 0 ) ]
		];
		newfile		= FileNameJoin @ { ParentDirectory @ file, newfile };
		RenameFile[ file, newfile ]
	];


(*************************************
 *                                   *
 *        The actual install         *
 *                                   *
 *************************************)

InstallLieLink @ installDir


End[]

EndPackage[]