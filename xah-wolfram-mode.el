;;; xah-wolfram-mode.el --- Major mode for editing Wolfram Language. -*- coding: utf-8; lexical-binding: t; -*-

;; Copyright © 2021, 2024 by Xah Lee

;; Author: Xah Lee ( http://xahlee.info/ )
;; Version: 2.11.20240324104511
;; Created: 2021-07-24
;; Package-Requires: ((emacs "27"))
;; Keywords: languages, Wolfram Language, Mathematica
;; Homepage: http://xahlee.info/emacs/misc/xah-wolfram-mode.html

;; This file is not part of GNU Emacs.

;;; Commentary:

;; Major mode for editing Wolfram Language (aka Mathematica) code

;;; Installation:
;; here's how to manual install
;;
;; put the file
;; xah-wolfram-mode.el
;; in
;; ~/.emacs.d/lisp/
;; create the dir if doesn't exist.
;;
;; put the following in your emacs init file:
;;
;; (add-to-list 'load-path "~/.emacs.d/lisp/")
;; (require 'xah-wolfram-mode)

;;; How to use:
;;
;; M-x xah-wolfram-mode to toggle the mode on/off.
;; M-x describe-function then xah-wolfram-mode to see documentation.

;;; Customization:
;;
;; all keybinding for this mode are key sequences, starting with a leader key stored in this variable
;; xah-major-mode-leader-key
;; by default, it's TAB key.
;; you can change it by put this in your emacs init, before loading the mode
;; (setq xah-major-mode-leader-key (kbd "<f6>"))


;;; Code:

(defun xah-get-pos-of-block ()
  "Return the [begin end] positions of current text block.
Return value is a vector.
Version: 2024-03-23"
  (let (xp1 xp2 (xblankRegex "\n[ \t]*\n"))
    (save-excursion
      (setq xp1 (if (re-search-backward xblankRegex nil 1)
                    (goto-char (match-end 0))
                  (point)))
      (setq xp2 (if (re-search-forward xblankRegex nil 1)
                    (match-beginning 0)
                  (point))))
    (vector xp1 xp2)))

(defun xah-get-pos-of-block-or ()
  "If region is active, return its [begin end] positions, else same as `xah-get-pos-of-block'.
Return value is a vector.
Version: 2024-03-23"
  (if (region-active-p)
      (vector (region-beginning) (region-end))
    (xah-get-pos-of-block)))

(defun xah-wolfram-replace-regexp-pairs-region (Begin End Pairs &optional Fixedcase-p Literal-p Hilight-p)
  "Replace regex string find/replace Pairs in region.

Begin End are the region boundaries.

Pairs is
 [[regexStr1 replaceStr1] [regexStr2 replaceStr2] …]
It can be list or vector, for the elements or the entire argument.

The optional arguments Fixedcase-p and Literal-p is the same as in `replace-match'.
If Hilight-p is true, highlight the changed region.

Find strings case sensitivity depends on `case-fold-search'. You can set it locally, like this: (let ((case-fold-search nil)) …)
Version: 2017-02-21 2021-08-14"
  (save-excursion
    (save-restriction
      (narrow-to-region Begin End)
      (mapc
       (lambda (xx)
         (goto-char (point-min))
         (while (re-search-forward (elt xx 0) (point-max) t)
           (replace-match (elt xx 1) Fixedcase-p Literal-p)
           (when Hilight-p
             (overlay-put (make-overlay (match-beginning 0) (match-end 0)) 'face 'highlight))))
       Pairs))))

(defvar xah-wolfram-temp-dir-path nil "Path to temp dir used by xah commands.
by default, the value is (concat user-emacs-directory \"temp/\").")

(setq xah-wolfram-temp-dir-path (expand-file-name (concat user-emacs-directory "temp/")))

(defun xah-wolfram-eval-region (Xbegin Xend)
  "Eval code in text selection with WolframScript.
Version: 2024-03-21"
  (interactive "r")
  (let ((xcode (buffer-substring-no-properties Xbegin Xend))
        (xrandfilename
         (concat
          (if xah-wolfram-temp-dir-path
              xah-wolfram-temp-dir-path
            temporary-file-directory
            )
          (format "wolfram_%s_%x.wls" (format-time-string "%Y%m%d%-H%M%S") (random #xfffff))))
        (xoutBuf "*Wolfram output*")
        xcmdStr
        )
    (setq xcmdStr (format  "wolframscript -print all -file %s &" xrandfilename))
    (with-temp-file xrandfilename
      (insert xcode))
    (message "Running 「%s」" xcmdStr)
    (shell-command xcmdStr xoutBuf)
    (display-buffer xoutBuf)))

(defun xah-wolfram-eval-current-line ()
  "Eval the current line with WolframScript.
Version: 2024-03-17 2024-03-21"
  (interactive)
  (xah-wolfram-eval-region (line-beginning-position) (line-end-position)))

(defun xah-wolfram-run-script (Filepath &optional OptStr)
  "Execute the current file with WolframScript and print last expression.
The current file should have one of the filename extension: wl wls.

When `universal-argument' is called first, prompt user to give WolframScript command line options. (「-file name」 is always used.)

If the file is modified, save it automatically before run.
Version: 2021-10-27 2024-03-21 2024-03-22"
  (interactive
   (let (xpromptAnswer)
     (when (not buffer-file-name) (user-error "Buffer is not a file. Save it first"))
     (when (buffer-modified-p) (save-buffer))
     (setq xpromptAnswer
           (if current-prefix-arg
               (read-string "WolframScript additional options:")
             nil
             ))
     (list buffer-file-name xpromptAnswer)))
  (let (xcmdStr (xoutBuf "*Wolfram output*"))
    (setq xcmdStr (format  "wolframscript -print -file %s %s &" (shell-quote-argument Filepath) OptStr))
    (message "Running 「%s」" xcmdStr)
    (shell-command xcmdStr xoutBuf)
    (display-buffer xoutBuf)))

(defun xah-wolfram-run-script-print-last ()
  "Run the current file with WolframScript, print last expression.
calls `xah-wolfram-run-script'.
Version: 2024-03-21"
  (interactive)
  (xah-wolfram-run-script buffer-file-name "-print"))

(defun xah-wolfram-run-script-print-all ()
  "Run the current file with WolframScript with option -print all.
calls `xah-wolfram-run-script'.
Version: 2021-10-27 2024-03-21"
  (interactive)
  (xah-wolfram-run-script buffer-file-name "-print all"))

(defvar xah-wolfram-special-char
'(

"\\[FormalA]"
"\\[FormalB]"
"\\[FormalC]"
"\\[FormalD]"
"\\[FormalE]"
"\\[FormalF]"
"\\[FormalG]"
"\\[FormalH]"
"\\[FormalI]"
"\\[FormalJ]"
"\\[FormalK]"
"\\[FormalL]"
"\\[FormalM]"
"\\[FormalN]"
"\\[FormalO]"
"\\[FormalP]"
"\\[FormalQ]"
"\\[FormalR]"
"\\[FormalS]"
"\\[FormalT]"
"\\[FormalU]"
"\\[FormalV]"
"\\[FormalW]"
"\\[FormalX]"
"\\[FormalY]"
"\\[FormalZ]"

"\\[FormalCapitalA]"
"\\[FormalCapitalB]"
"\\[FormalCapitalC]"
"\\[FormalCapitalD]"
"\\[FormalCapitalE]"
"\\[FormalCapitalF]"
"\\[FormalCapitalG]"
"\\[FormalCapitalH]"
"\\[FormalCapitalI]"
"\\[FormalCapitalJ]"
"\\[FormalCapitalK]"
"\\[FormalCapitalL]"
"\\[FormalCapitalM]"
"\\[FormalCapitalN]"
"\\[FormalCapitalO]"
"\\[FormalCapitalP]"
"\\[FormalCapitalQ]"
"\\[FormalCapitalR]"
"\\[FormalCapitalS]"
"\\[FormalCapitalT]"
"\\[FormalCapitalU]"
"\\[FormalCapitalV]"
"\\[FormalCapitalW]"
"\\[FormalCapitalX]"
"\\[FormalCapitalY]"
"\\[FormalCapitalZ]"

"\\[FormalCapitalAlpha]"
"\\[FormalCapitalBeta]"
"\\[FormalCapitalGamma]"
"\\[FormalCapitalDelta]"
"\\[FormalCapitalEpsilon]"
"\\[FormalCapitalZeta]"
"\\[FormalCapitalEta]"
"\\[FormalCapitalTheta]"
"\\[FormalCapitalIota]"
"\\[FormalCapitalKappa]"
"\\[FormalCapitalLambda]"
"\\[FormalCapitalMu]"
"\\[FormalCapitalNu]"
"\\[FormalCapitalXi]"
"\\[FormalCapitalOmicron]"
"\\[FormalCapitalPi]"
"\\[FormalCapitalRho]"
"\\[FormalCapitalSigma]"
"\\[FormalCapitalTau]"
"\\[FormalCapitalUpsilon]"
"\\[FormalCapitalPhi]"
"\\[FormalCapitalChi]"
"\\[FormalCapitalPsi]"
"\\[FormalCapitalOmega]"

"\\[FormalAlpha]"
"\\[FormalBeta]"
"\\[FormalGamma]"
"\\[FormalDelta]"
"\\[FormalCurlyEpsilon]"
"\\[FormalZeta]"
"\\[FormalEta]"
"\\[FormalTheta]"
"\\[FormalIota]"
"\\[FormalKappa]"
"\\[FormalLambda]"
"\\[FormalMu]"
"\\[FormalNu]"
"\\[FormalXi]"
"\\[FormalOmicron]"
"\\[FormalPi]"
"\\[FormalRho]"
"\\[FormalFinalSigma]"
"\\[FormalSigma]"
"\\[FormalTau]"
"\\[FormalUpsilon]"
"\\[FormalCurlyPhi]"
"\\[FormalChi]"
"\\[FormalPsi]"
"\\[FormalOmega]"
"\\[FormalCurlyTheta]"
"\\[FormalCurlyCapitalUpsilon]"
"\\[FormalPhi]"
"\\[FormalCurlyPi]"
"\\[FormalCapitalStigma]"
"\\[FormalStigma]"
"\\[FormalCapitalDigamma]"
"\\[FormalDigamma]"
"\\[FormalCapitalKoppa]"
"\\[FormalKoppa]"
"\\[FormalCapitalSampi]"
"\\[FormalSampi]"
"\\[FormalCurlyKappa]"
"\\[FormalCurlyRho]"
"\\[FormalEpsilon]"

)
 "List of Wolfram Language named characters.")

(defvar xah-wolfram-funs1 nil "List of Wolfram Language symbols. Part of many.")

(setq xah-wolfram-funs1
'( "AASTriangle" "AbelianGroup" "Abort" "AbortKernels" "AbortProtect"
"AbortScheduledTask" "Above" "Abs" "AbsArg" "AbsArgPlot" "Absolute"
"AbsoluteCorrelation" "AbsoluteCorrelationFunction"
"AbsoluteCurrentValue" "AbsoluteDashing" "AbsoluteFileName"
"AbsoluteOptions" "AbsolutePointSize" "AbsoluteThickness"
"AbsoluteTime" "AbsoluteTiming" "AcceptanceThreshold" "AccountingForm"
"Accumulate" "Accuracy" "AccuracyGoal" "AcousticAbsorbingValue"
"AcousticImpedanceValue" "AcousticNormalVelocityValue"
"AcousticPDEComponent" "AcousticPressureCondition"
"AcousticRadiationValue" "AcousticSoundHardValue"
"AcousticSoundSoftCondition" "ActionDelay" "ActionMenu"
"ActionMenuBox" "ActionMenuBoxOptions" "Activate" "Active"
"ActiveClassification" "ActiveClassificationObject" "ActiveItem"
"ActivePrediction" "ActivePredictionObject" "ActiveStyle"
"AcyclicGraphQ" "AddDocumentationDirectory" "AddDocumentationNotebook"
"AddOnHelpPath" "AddSides" "AddTo" "AddToSearchIndex" "AddUsers"
"AdjacencyGraph" "AdjacencyList" "AdjacencyMatrix" "AdjacentMeshCells"
"AdjustmentBox" "AdjustmentBoxOptions" "AdjustTimeSeriesForecast"
"AdministrativeDivisionData" "AffineHalfSpace" "AffineSpace"
"AffineStateSpaceModel" "AffineTransform" "After"
"AggregatedEntityClass" "AggregationLayer" "AircraftData"
"AirportData" "AirPressureData" "AirSoundAttenuation"
"AirTemperatureData" "AiryAi" "AiryAiPrime" "AiryAiZero" "AiryBi"
"AiryBiPrime" "AiryBiZero" "AlgebraicIntegerQ" "AlgebraicNumber"
"AlgebraicNumberDenominator" "AlgebraicNumberNorm"
"AlgebraicNumberPolynomial" "AlgebraicNumberTrace" "AlgebraicRules"
"AlgebraicRulesData" "Algebraics" "AlgebraicUnitQ" "Alignment"
"AlignmentMarker" "AlignmentPoint" "All" "AllowAdultContent"
"AllowedCloudExtraParameters" "AllowedCloudParameterExtensions"
"AllowedDimensions" "AllowedFrequencyRange" "AllowedHeads"
"AllowGroupClose" "AllowIncomplete" "AllowInlineCells"
"AllowKernelInitialization" "AllowLooseGrammar"
"AllowReverseGroupClose" "AllowScriptLevelChange" "AllowVersionUpdate"
"AllTrue" "Alphabet" "AlphabeticOrder" "AlphabeticSort" "AlphaChannel"
"AlternateImage" "AlternatingFactorial" "AlternatingGroup"
"AlternativeHypothesis" "Alternatives" "AltitudeMethod" "AmbientLight"
"AmbiguityFunction" "AmbiguityList" "Analytic" "AnatomyData"
"AnatomyForm" "AnatomyPlot3D" "AnatomySkinStyle" "AnatomyStyling"
"AnchoredSearch" "And" "AndersonDarlingTest" "AngerJ" "AngleBisector"
"AngleBracket" "AnglePath" "AnglePath3D" "AngleVector" "AngularGauge"
"Animate" "AnimatedImage" "AnimationCycleOffset"
"AnimationCycleRepetitions" "AnimationDirection"
"AnimationDisplayTime" "AnimationRate" "AnimationRepetitions"
"AnimationRunning" "AnimationRunTime" "AnimationTimeIndex" "Animator"
"AnimatorBox" "AnimatorBoxOptions" "AnimatorElements" "Annotate"
"Annotation" "AnnotationDelete" "AnnotationKeys" "AnnotationRules"
"AnnotationValue" "Annuity" "AnnuityDue" "Annulus" "AnomalyDetection"
"AnomalyDetector" "AnomalyDetectorFunction" "Anonymous" "Antialiasing"
"Antihermitian" "AntihermitianMatrixQ" "Antisymmetric"
"AntisymmetricMatrixQ" "Antonyms" "AnyOrder" "AnySubset" "AnyTrue"
"Apart" "ApartSquareFree" "APIFunction" "Appearance"
"AppearanceElements" "AppearanceRules" "AppellF1" "Append"
"AppendCheck" "AppendLayer" "AppendTo" "Application" "ApplicationAdd"
"ApplicationDataDirectory" "ApplicationDataUserDirectory"
"ApplicationDirectoriesLocate" "ApplicationDirectoryAdd"
"ApplicationIdentificationKey" "ApplicationsLocate" "Apply"
"ApplySides" "ApplyTo" "ArcCos" "ArcCosh" "ArcCot" "ArcCoth" "ArcCsc"
"ArcCsch" "ArcCurvature" "ARCHProcess" "ArcLength" "ArcSec" "ArcSech"
"ArcSin" "ArcSinDistribution" "ArcSinh" "ArcTan" "ArcTanh" "Area"
"Arg" "ArgMax" "ArgMin" "ArgumentCountQ" "ArgumentsOptions"
"ARIMAProcess" "ArithmeticGeometricMean" "ARMAProcess" "Around"
"AroundReplace" "ARProcess" "Array" "ArrayComponents" "ArrayDepth"
"ArrayFilter" "ArrayFlatten" "ArrayMesh" "ArrayPad" "ArrayPlot"
"ArrayPlot3D" "ArrayQ" "ArrayReduce" "ArrayResample" "ArrayReshape"
"ArrayRules" "Arrays" "Arrow" "Arrow3DBox" "ArrowBox" "Arrowheads"
"ASATriangle" "Ask" "AskAppend" "AskConfirm" "AskDisplay" "AskedQ"
"AskedValue" "AskFunction" "AskState" "AskTemplateDisplay"
"AspectRatio" "AspectRatioFixed" "Assert" "AssessmentFunction"
"AssessmentResultObject" "AssociateTo" "Association"
"AssociationFormat" "AssociationMap" "AssociationQ"
"AssociationThread" "AssumeDeterministic" "Assuming" "Assumptions"
"AstronomicalData" "Asymptotic" "AsymptoticDSolveValue"
"AsymptoticEqual" "AsymptoticEquivalent" "AsymptoticExpectation"
"AsymptoticGreater" "AsymptoticGreaterEqual" "AsymptoticIntegrate"
"AsymptoticLess" "AsymptoticLessEqual" "AsymptoticOutputTracker"
"AsymptoticProbability" "AsymptoticProduct" "AsymptoticRSolveValue"
"AsymptoticSolve" "AsymptoticSum" "Asynchronous"
"AsynchronousTaskObject" "AsynchronousTasks" "Atom" "AtomCoordinates"
"AtomCount" "AtomDiagramCoordinates" "AtomList" "AtomQ" "AttachCell"
"AttentionLayer" "Attributes" "Audio" "AudioAmplify" "AudioAnnotate"
"AudioAnnotationLookup" "AudioBlockMap" "AudioCapture"
"AudioChannelAssignment" "AudioChannelCombine" "AudioChannelMix"
"AudioChannels" "AudioChannelSeparate" "AudioData" "AudioDelay"
"AudioDelete" "AudioDevice" "AudioDistance" "AudioEncoding"
"AudioFade" "AudioFrequencyShift" "AudioGenerator" "AudioIdentify"
"AudioInputDevice" "AudioInsert" "AudioInstanceQ" "AudioIntervals"
"AudioJoin" "AudioLabel" "AudioLength" "AudioLocalMeasurements"
"AudioLooping" "AudioLoudness" "AudioMeasurements" "AudioNormalize"
"AudioOutputDevice" "AudioOverlay" "AudioPad" "AudioPan"
"AudioPartition" "AudioPause" "AudioPitchShift" "AudioPlay"
"AudioPlot" "AudioQ" "AudioRecord" "AudioReplace" "AudioResample"
"AudioReverb" "AudioReverse" "AudioSampleRate" "AudioSpectralMap"
"AudioSpectralTransformation" "AudioSplit" "AudioStop" "AudioStream"
"AudioStreams" "AudioTimeStretch" "AudioTrackApply"
"AudioTrackSelection" "AudioTrim" "AudioType" "AugmentedPolyhedron"
"AugmentedSymmetricPolynomial" "Authenticate" "Authentication"
"AuthenticationDialog" "AutoAction" "Autocomplete"
"AutocompletionFunction" "AutoCopy" "AutocorrelationTest" "AutoDelete"
"AutoEvaluateEvents" "AutoGeneratedPackage" "AutoIndent"
"AutoIndentSpacings" "AutoItalicWords" "AutoloadPath" "AutoMatch"
"Automatic" "AutomaticImageSize" "AutoMultiplicationSymbol"
"AutoNumberFormatting" "AutoOpenNotebooks" "AutoOpenPalettes"
"AutoQuoteCharacters" "AutoRefreshed" "AutoRemove" "AutorunSequencing"
"AutoScaling" "AutoScroll" "AutoSpacing" "AutoStyleOptions"
"AutoStyleWords" "AutoSubmitting" "Axes" "AxesEdge" "AxesLabel"
"AxesOrigin" "AxesStyle" "AxiomaticTheory" "Axis" "Axis3DBox"
"AxisBox" "BabyMonsterGroupB" "Back" "Background"
"BackgroundAppearance" "BackgroundTasksSettings" "Backslash"
"Backsubstitution" "Backward" "Ball" "Band" "BandpassFilter"
"BandstopFilter" "BarabasiAlbertGraphDistribution" "BarChart"
"BarChart3D" "BarcodeImage" "BarcodeRecognize" "BaringhausHenzeTest"
"BarLegend" "BarlowProschanImportance" "BarnesG" "BarOrigin"
"BarSpacing" "BartlettHannWindow" "BartlettWindow" "BaseDecode"
"BaseEncode" "BaseForm" "Baseline" "BaselinePosition" "BaseStyle"
"BasicRecurrentLayer" "BatchNormalizationLayer" "BatchSize"
"BatesDistribution" "BattleLemarieWavelet" "BayesianMaximization"
"BayesianMaximizationObject" "BayesianMinimization"
"BayesianMinimizationObject" "Because" "BeckmannDistribution" "Beep"
"Before" "Begin" "BeginDialogPacket" "BeginFrontEndInteractionPacket"
"BeginPackage" "BellB" "BellY" "Below" "BenfordDistribution"
"BeniniDistribution" "BenktanderGibratDistribution"
"BenktanderWeibullDistribution" "BernoulliB" "BernoulliDistribution"
"BernoulliGraphDistribution" "BernoulliProcess" "BernsteinBasis"
"BesagL" "BesselFilterModel" "BesselI" "BesselJ" "BesselJZero"
"BesselK" "BesselY" "BesselYZero" "Beta" "BetaBinomialDistribution"
"BetaDistribution" "BetaNegativeBinomialDistribution"
"BetaPrimeDistribution" "BetaRegularized" "Between"
"BetweennessCentrality" "BeveledPolyhedron" "BezierCurve"
"BezierCurve3DBox" "BezierCurve3DBoxOptions" "BezierCurveBox"
"BezierCurveBoxOptions" "BezierFunction" "BilateralFilter" "Binarize"
"BinaryDeserialize" "BinaryDistance" "BinaryFormat" "BinaryImageQ"
"BinaryRead" "BinaryReadList" "BinarySerialize" "BinaryWrite"
"BinCounts" "BinLists" "Binomial" "BinomialDistribution"
"BinomialPointProcess" "BinomialProcess" "BinormalDistribution"
"BiorthogonalSplineWavelet" "BioSequence"
"BioSequenceBackTranslateList" "BioSequenceComplement"
"BioSequenceInstances" "BioSequenceModify" "BioSequenceQ"
"BioSequenceReverseComplement" "BioSequenceTranscribe"
"BioSequenceTranslate" "BipartiteGraphQ" "BiquadraticFilterModel"
"BirnbaumImportance" "BirnbaumSaundersDistribution" "BitAnd"
"BitClear" "BitDepth" "BitGet" "BitLength" "BitNot" "BitOr" "BitSet"
"BitShiftLeft" "BitShiftRight" "BitXor" "BiweightLocation"
"BiweightMidvariance" "Black" "BlackmanHarrisWindow"
"BlackmanNuttallWindow" "BlackmanWindow" "Blank" "BlankForm"
"BlankNullSequence" "BlankSequence" "Blend" "Block"
"BlockchainAddressData" "BlockchainBase" "BlockchainBlockData"
"BlockchainContractValue" "BlockchainData" "BlockchainGet"
"BlockchainKeyEncode" "BlockchainPut" "BlockchainTokenData"
"BlockchainTransaction" "BlockchainTransactionData"
"BlockchainTransactionSign" "BlockchainTransactionSubmit" "BlockMap"
"BlockRandom" "BlomqvistBeta" "BlomqvistBetaTest" "Blue" "Blur"
"BodePlot" "BohmanWindow" "Bold" "Bond" "BondCount" "BondList" "BondQ"
"Bookmarks" "Boole" "BooleanConsecutiveFunction" "BooleanConvert"
"BooleanCountingFunction" "BooleanFunction" "BooleanGraph"
"BooleanMaxterms" "BooleanMinimize" "BooleanMinterms" "BooleanQ"
"BooleanRegion" "Booleans" "BooleanStrings" "BooleanTable"
"BooleanVariables" "BorderDimensions" "BorelTannerDistribution"
"Bottom" "BottomHatTransform" "BoundaryDiscretizeGraphics"
"BoundaryDiscretizeRegion" "BoundaryMesh" "BoundaryMeshRegion"
"BoundaryMeshRegionQ" "BoundaryStyle" "BoundedRegionQ"
"BoundingRegion" "Bounds" "Box" "BoxBaselineShift" "BoxData"
"BoxDimensions" "Boxed" "Boxes" "BoxForm" "BoxFormFormatTypes"
"BoxFrame" "BoxID" "BoxMargins" "BoxMatrix" "BoxObject" "BoxRatios"
"BoxRotation" "BoxRotationPoint" "BoxStyle" "BoxWhiskerChart" "Bra"
"BracketingBar" "BraKet" "BrayCurtisDistance" "BreadthFirstScan"
"Break" "BridgeData" "BrightnessEqualize" "BroadcastStationData"
"Brown" "BrownForsytheTest" "BrownianBridgeProcess" "BrowserCategory"
"BSplineBasis" "BSplineCurve" "BSplineCurve3DBox"
"BSplineCurve3DBoxOptions" "BSplineCurveBox" "BSplineCurveBoxOptions"
"BSplineFunction" "BSplineSurface" "BSplineSurface3DBox"
"BSplineSurface3DBoxOptions" "BubbleChart" "BubbleChart3D"
"BubbleScale" "BubbleSizes" "BuildingData" "BulletGauge"
"BusinessDayQ" "ButterflyGraph" "ButterworthFilterModel" "Button"
"ButtonBar" "ButtonBox" "ButtonBoxOptions" "ButtonCell"
"ButtonContents" "ButtonData" "ButtonEvaluator" "ButtonExpandable"
"ButtonFrame" "ButtonFunction" "ButtonMargins" "ButtonMinHeight"
"ButtonNote" "ButtonNotebook" "ButtonSource" "ButtonStyle"
"ButtonStyleMenuListing" "Byte" "ByteArray" "ByteArrayFormat"
"ByteArrayFormatQ" "ByteArrayQ" "ByteArrayToString" "ByteCount"
"ByteOrdering" "C" "CachedValue" "CacheGraphics" "CachePersistence"
"CalendarConvert" "CalendarData" "CalendarType" "Callout"
"CalloutMarker" "CalloutStyle" "CallPacket" "CanberraDistance"
"Cancel" "CancelButton" "CandlestickChart" "CanonicalGraph"
"CanonicalizePolygon" "CanonicalizePolyhedron" "CanonicalName"
"CanonicalWarpingCorrespondence" "CanonicalWarpingDistance"
"CantorMesh" "CantorStaircase" "Canvas" "Cap" "CapForm"
"CapitalDifferentialD" "Capitalize" "CapsuleShape" "CaptureRunning"
"CardinalBSplineBasis" "CarlemanLinearize" "CarmichaelLambda"
"CaseOrdering" "Cases" "CaseSensitive" "Cashflow" "Casoratian"
"Catalan" "CatalanNumber" "Catch" "CategoricalDistribution" "Catenate"
"CatenateLayer" "CauchyDistribution" "CauchyPointProcess"
"CauchyWindow" "CayleyGraph" "CDF" "CDFDeploy" "CDFInformation"
"CDFWavelet" "Ceiling" "CelestialSystem" "Cell" "CellAutoOverwrite"
"CellBaseline" "CellBoundingBox" "CellBracketOptions"
"CellChangeTimes" "CellContents" "CellContext" "CellDingbat"
"CellDynamicExpression" "CellEditDuplicate" "CellElementsBoundingBox"
"CellElementSpacings" "CellEpilog" "CellEvaluationDuplicate"
"CellEvaluationFunction" "CellEvaluationLanguage" "CellEventActions"
"CellFrame" "CellFrameColor" "CellFrameLabelMargins" "CellFrameLabels"
"CellFrameMargins" "CellGroup" "CellGroupData" "CellGrouping"
"CellGroupingRules" "CellHorizontalScrolling" "CellID" "CellLabel"
"CellLabelAutoDelete" "CellLabelMargins" "CellLabelPositioning"
"CellLabelStyle" "CellLabelTemplate" "CellMargins" "CellObject"
"CellOpen" "CellPrint" "CellProlog" "Cells" "CellSize" "CellStyle"
"CellTags" "CellularAutomaton" "CensoredDistribution" "Censoring"
"Center" "CenterArray" "CenterDot" "CentralFeature" "CentralMoment"
"CentralMomentGeneratingFunction" "Cepstrogram" "CepstrogramArray"
"CepstrumArray" "CForm" "ChampernowneNumber" "ChangeOptions"
"ChannelBase" "ChannelBrokerAction" "ChannelDatabin"
"ChannelHistoryLength" "ChannelListen" "ChannelListener"
"ChannelListeners" "ChannelListenerWait" "ChannelObject"
"ChannelPreSendFunction" "ChannelReceiverFunction" "ChannelSend"
"ChannelSubscribers" "ChanVeseBinarize" "Character" "CharacterCounts"
"CharacterEncoding" "CharacterEncodingsPath" "CharacteristicFunction"
"CharacteristicPolynomial" "CharacterName" "CharacterNormalize"
"CharacterRange" "Characters" "ChartBaseStyle" "ChartElementData"
"ChartElementDataFunction" "ChartElementFunction" "ChartElements"
"ChartLabels" "ChartLayout" "ChartLegends" "ChartStyle"
"Chebyshev1FilterModel" "Chebyshev2FilterModel" "ChebyshevDistance"
"ChebyshevT" "ChebyshevU" "Check" "CheckAbort" "CheckAll"
"CheckArguments" "Checkbox" "CheckboxBar" "CheckboxBox"
"CheckboxBoxOptions" "ChemicalData" "ChessboardDistance"
"ChiDistribution" "ChineseRemainder" "ChiSquareDistribution"
"ChoiceButtons" "ChoiceDialog" "CholeskyDecomposition" "Chop"
"ChromaticityPlot" "ChromaticityPlot3D" "ChromaticPolynomial" "Circle"
"CircleBox" "CircleDot" "CircleMinus" "CirclePlus" "CirclePoints"
"CircleThrough" "CircleTimes" "CirculantGraph"
"CircularOrthogonalMatrixDistribution"
"CircularQuaternionMatrixDistribution"
"CircularRealMatrixDistribution"
"CircularSymplecticMatrixDistribution"
"CircularUnitaryMatrixDistribution" "Circumsphere" "CityData"
"ClassifierFunction" "ClassifierInformation" "ClassifierMeasurements"
"ClassifierMeasurementsObject" "Classify" "ClassPriors" "Clear"
"ClearAll" "ClearAttributes" "ClearCookies" "ClearPermissions"
"ClearSystemCache" "ClebschGordan" "ClickPane" "ClickToCopy"
"ClickToCopyEnabled" "Clip" "ClipboardNotebook" "ClipFill"
"ClippingStyle" "ClipPlanes" "ClipPlanesStyle" "ClipRange" "Clock"
"ClockGauge" "ClockwiseContourIntegral" "Close" "Closed"
"CloseDocumentationIndex" "CloseDocumentationIndexMerger"
"CloseDocumentationNotebookIndexer" "CloseKernels"
"ClosenessCentrality" "Closing" "ClosingAutoSave" "ClosingEvent"
"CloudAccountData" "CloudBase" "CloudConnect" "CloudConnections"
"CloudDeploy" "CloudDirectory" "CloudDisconnect" "CloudEvaluate"
"CloudExport" "CloudExpression" "CloudExpressions" "CloudFunction"
"CloudGet" "CloudImport" "CloudLoggingData" "CloudObject"
"CloudObjectInformation" "CloudObjectInformationData"
"CloudObjectNameFormat" "CloudObjects" "CloudObjectURLType"
"CloudPublish" "CloudPut" "CloudRenderingMethod" "CloudSave"
"CloudShare" "CloudSubmit" "CloudSymbol" "CloudUnshare"
"CloudUsageData" "CloudUserID" "ClusterClassify"
"ClusterDissimilarityFunction" "ClusteringComponents" "ClusteringTree"
"CMYKColor" "Coarse" "CodeAssistOptions" "Coefficient"
"CoefficientArrays" "CoefficientDomain" "CoefficientList"
"CoefficientRules" "CoifletWavelet" "Collect" "CollinearPoints"
"Colon" "ColonForm" "ColorBalance" "ColorCombine" "ColorConvert"
"ColorCoverage" "ColorData" "ColorDataFunction" "ColorDetect"
"ColorDistance" "ColorFunction" "ColorFunctionBinning"
"ColorFunctionScaling" "Colorize" "ColorNegate" "ColorOutput"
"ColorProfileData" "ColorQ" "ColorQuantize" "ColorReplace"
"ColorRules" "ColorSelectorSettings" "ColorSeparate" "ColorSetter"
"ColorSetterBox" "ColorSetterBoxOptions" "ColorSlider" "ColorsNear"
"ColorSpace" "ColorToneMapping" "Column" "ColumnAlignments"
"ColumnBackgrounds" "ColumnForm" "ColumnLines" "ColumnsEqual"
"ColumnSpacings" "ColumnWidths" "CombinatorB" "CombinatorC"
"CombinatorI" "CombinatorK" "CombinatorS" "CombinatorW" "CombinatorY"
"CombinedEntityClass" "CombinerFunction" "CometData"
"CommonDefaultFormatTypes" "Commonest" "CommonestFilter" "CommonName"
"CommonUnits" "CommunityBoundaryStyle" "CommunityGraphPlot"
"CommunityLabels" "CommunityRegionStyle" "CompanyData"
"CompatibleUnitQ" "CompilationOptions" "CompilationTarget" "Compile"
"Compiled" "CompiledCodeFunction" "CompiledFunction" "CompiledLayer"
"CompilerOptions" "Complement" "ComplementedEntityClass"
"CompleteGraph" "CompleteGraphQ" "CompleteKaryTree"
"CompletionsListPacket" "Complex" "ComplexArrayPlot"
"ComplexContourPlot" "Complexes" "ComplexExpand" "ComplexInfinity"
"ComplexityFunction" "ComplexListPlot" "ComplexPlot" "ComplexPlot3D"
"ComplexRegionPlot" "ComplexStreamPlot" "ComplexVectorPlot"
"ComponentMeasurements" "ComponentwiseContextMenu"
"ComposeList" "ComposeSeries" "CompositeQ" "Composition"
"CompoundElement" "CompoundExpression" "CompoundPoissonDistribution"
"CompoundPoissonProcess" "CompoundRenewalProcess" "Compress"
"CompressedData" "CompressionLevel" "ComputeUncertainty" "Condition"
"ConditionalExpression" "Conditioned" "Cone" "ConeBox"
"ConfidenceLevel" "ConfidenceRange" "ConfidenceTransform"
"ConfigurationPath" "Confirm" "ConfirmAssert" "ConfirmBy"
"ConfirmMatch" "ConfirmQuiet" "ConformAudio" "ConformImages"
"Congruent" "ConicGradientFilling" "ConicHullRegion"
"ConicHullRegion3DBox" "ConicHullRegionBox" "ConicOptimization"
"Conjugate" "ConjugateTranspose" "Conjunction" "Connect"
"ConnectedComponents" "ConnectedGraphComponents" "ConnectedGraphQ"
"ConnectedMeshComponents" "ConnectedMoleculeComponents"
"ConnectedMoleculeQ" "ConnectionSettings"
"ConnectLibraryCallbackFunction" "ConnectSystemModelComponents"
"ConnesWindow" "ConoverTest" "ConservativeConvectionPDETerm"
"ConsoleMessage" "ConsoleMessagePacket" "Constant" "ConstantArray"
"ConstantArrayLayer" "ConstantImage" "ConstantPlusLayer"
"ConstantRegionQ" "Constants" "ConstantTimesLayer" "ConstellationData"
"ConstrainedMax" "ConstrainedMin" "Construct" "Containing"
"ContainsAll" "ContainsAny" "ContainsExactly" "ContainsNone"
"ContainsOnly" "ContentFieldOptions" "ContentLocationFunction"
"ContentObject" "ContentPadding" "ContentsBoundingBox"
"ContentSelectable" "ContentSize" "Context" "ContextMenu" "Contexts"
"ContextToFileName" "Continuation" "Continue" "ContinuedFraction"
"ContinuedFractionK" "ContinuousAction" "ContinuousMarkovProcess"
"ContinuousTask" "ContinuousTimeModelQ" "ContinuousWaveletData"
"ContinuousWaveletTransform" "ContourDetect" "ContourGraphics"
"ContourIntegral" "ContourLabels" "ContourLines" "ContourPlot"
"ContourPlot3D" "Contours" "ContourShading" "ContourSmoothing"
"ContourStyle" "ContraharmonicMean" "ContrastiveLossLayer" "Control"
"ControlActive" "ControlAlignment" "ControlGroupContentsBox"
"ControllabilityGramian" "ControllabilityMatrix"
"ControllableDecomposition" "ControllableModelQ" "ControllerDuration"
"ControllerInformation" "ControllerInformationData"
"ControllerLinking" "ControllerManipulate" "ControllerMethod"
"ControllerPath" "ControllerState" "ControlPlacement"
"ControlsRendering" "ControlType" "ConvectionPDETerm" "Convergents"
"ConversionOptions" "ConversionRules" "ConvertToBitmapPacket"
"ConvertToPostScript" "ConvertToPostScriptPacket" "ConvexHullMesh"
"ConvexHullRegion" "ConvexOptimization" "ConvexPolygonQ"
"ConvexPolyhedronQ" "ConvexRegionQ" "ConvolutionLayer" "Convolve"
"ConwayGroupCo1" "ConwayGroupCo2" "ConwayGroupCo3" "CookieFunction"
"Cookies" "CoordinateBoundingBox" "CoordinateBoundingBoxArray"
"CoordinateBounds" "CoordinateBoundsArray" "CoordinateChartData"
"CoordinatesToolOptions" "CoordinateTransform"
"CoordinateTransformData" "CoplanarPoints" "CoprimeQ" "Coproduct"
"CopulaDistribution" "Copyable" "CopyDatabin" "CopyDirectory"
"CopyFile" "CopyFunction" "CopyTag" "CopyToClipboard" "CornerFilter"
"CornerNeighbors" "Correlation" "CorrelationDistance"
"CorrelationFunction" "CorrelationTest" "Cos" "Cosh" "CoshIntegral"
"CosineDistance" "CosineWindow" "CosIntegral" "Cot" "Coth" "Count"
"CountDistinct" "CountDistinctBy" "CounterAssignments" "CounterBox"
"CounterBoxOptions" "CounterClockwiseContourIntegral"
"CounterEvaluator" "CounterFunction" "CounterIncrements"
"CounterStyle" "CounterStyleMenuListing" "CountRoots" "CountryData"
"Counts" "CountsBy" "Covariance" "CovarianceEstimatorFunction"
"CovarianceFunction" "CoxianDistribution" "CoxIngersollRossProcess"
"CoxModel" "CoxModelFit" "CramerVonMisesTest" "CreateArchive"
"CreateCellID" "CreateChannel" "CreateCloudExpression" "CreateDatabin"
"CreateDataDirectory" "CreateDataStructure" "CreateDataSystemModel"
"CreateDialog" "CreateDirectory" "CreateDocument"
"CreateDocumentationIndex" "CreateFile"
"CreateIntermediateDirectories" "CreateLicenseEntitlement"
"CreateManagedLibraryExpression" "CreateNotebook"
"CreatePacletArchive" "CreatePalette" "CreatePalettePacket"
"CreatePermissionsGroup" "CreateScheduledTask" "CreateSearchIndex"
"CreateSpellIndex" "CreateSystemModel" "CreateTemporary" "CreateUUID"
"CreateWindow" "CriterionFunction" "CriticalityFailureImportance"
"CriticalitySuccessImportance" "CriticalSection" "Cross"
"CrossEntropyLossLayer" "CrossingCount" "CrossingDetect"
"CrossingPolygon" "CrossMatrix" "Csc" "Csch" "CTCLossLayer" "Cube"
"CubeRoot" "Cubics" "Cuboid" "CuboidBox" "Cumulant"
"CumulantGeneratingFunction" "Cup" "CupCap" "Curl" "CurlyDoubleQuote"
"CurlyQuote" "CurrencyConvert" "CurrentDate" "CurrentImage"
"CurrentlySpeakingPacket" "CurrentNotebookImage" "CurrentScreenImage"
"CurrentValue" "Curry" "CurryApplied" "CurvatureFlowFilter"
"CurveClosed" "Cyan" "CycleGraph" "CycleIndexPolynomial" "Cycles"
"CyclicGroup" "Cyclotomic" "Cylinder" "CylinderBox"
"CylindricalDecomposition" "CylindricalDecompositionFunction"
"DagumDistribution" "DamData" "DamerauLevenshteinDistance"
"DampingFactor" "Darker" "Dashed" "Dashing" "DatabaseAggregate"
"DatabaseAnnotate" "DatabaseComplement" "DatabaseComplementAll"
"DatabaseConnect" "DatabaseDisconnect" "DatabaseDistinct"
"DatabaseDistinctOn" "DatabaseEmbeddedQuery" "DatabaseExcludeFields"
"DatabaseFunction" "DatabaseGroupBy" "DatabaseIntersection"
"DatabaseIntersectionAll" "DatabaseJoin" "DatabaseLimit"
"DatabaseModel" "DatabaseModelInstance" "DatabaseOffset"
"DatabaseOrderBy" "DatabaseQueryMakeAlias"
"DatabaseQueryToSymbolicSQL" "DatabaseReference" "DatabaseReferences"
"DatabaseRunQuery" "Databases" "DatabaseSelectFields"
"DatabaseSetPrimaryKey" "DatabaseSQLAscending" "DatabaseSQLDescending"
"DatabaseStore" "DatabaseUnion" "DatabaseUnionAll" "DatabaseView"
"DatabaseWhere" "Databin" "DatabinAdd" "DatabinRemove" "Databins"
"DatabinSubmit" "DatabinUpload" "DataCompression" "DataDistribution"
"DataRange" "DataReversed" "Dataset" "DatasetDisplayPanel"
"DataStructure" "DataStructureQ" "Date" "DateBounds" "Dated"
"DateDelimiters" "DateDifference" "DatedUnit" "DateFormat"
"DateFunction" "DateHistogram" "DateInterval" "DateList"
"DateListLogPlot" "DateListPlot" "DateListStepPlot" "DateObject"
"DateObjectQ" "DateOverlapsQ" "DatePattern" "DatePlus" "DateRange"
"DateReduction" "DateSelect" "DateString" "DateTicksFormat"
"DateValue" "DateWithinQ" "DaubechiesWavelet" "DavisDistribution"
"DawsonF" "DayCount" "DayCountConvention" "DayHemisphere" "DaylightQ"
"DayMatchQ" "DayName" "DayNightTerminator" "DayPlus" "DayRange"
"DayRound" "DBEntityQueryToSQLString" "DeBruijnGraph"
"DeBruijnSequence" "Debug" "DebugTag" "Decapitalize" "Decimal"
"DecimalForm" "DeclareKnownSymbols" "DeclarePackage" "Decompose"
"DeconvolutionLayer" "Decrement" "Decrypt" "DecryptFile" "DedekindEta"
"DeepSpaceProbeData" "Default" "DefaultAxesStyle" "DefaultBaseStyle"
"DefaultBoxStyle" "DefaultButton" "DefaultColor"
"DefaultControlPlacement" "DefaultDuplicateCellStyle"
"DefaultDuration" "DefaultElement" "DefaultFaceGridsStyle"
"DefaultFieldHintStyle" "DefaultFont" "DefaultFontProperties"
"DefaultFormatType" "DefaultFormatTypeForStyle" "DefaultFrameStyle"
"DefaultFrameTicksStyle" "DefaultGridLinesStyle"
"DefaultInlineFormatType" "DefaultInputFormatType" "DefaultLabelStyle"
"DefaultMenuStyle" "DefaultNaturalLanguage" "DefaultNewCellStyle"
"DefaultNewInlineCellStyle" "DefaultNotebook" "DefaultOptions"
"DefaultOutputFormatType" "DefaultPrintPrecision" "DefaultStyle"
"DefaultStyleDefinitions" "DefaultTextFormatType"
"DefaultTextInlineFormatType" "DefaultTicksStyle"
"DefaultTooltipStyle" "DefaultValue" "DefaultValues" "Defer"
"DefineExternal" "DefineInputStreamMethod" "DefineOutputStreamMethod"
"DefineResourceFunction" "Definition" "Degree" "DegreeCentrality"
"DegreeGraphDistribution" "DegreeLexicographic"
"DegreeReverseLexicographic" "DEigensystem" "DEigenvalues"
"Deinitialization" "Del" "DelaunayMesh" "Delayed" "Deletable" "Delete"
"DeleteAnomalies" "DeleteBorderComponents" "DeleteCases"
"DeleteChannel" "DeleteCloudExpression" "DeleteContents"
"DeleteDirectory" "DeleteDuplicates" "DeleteDuplicatesBy" "DeleteFile"
"DeleteMissing" "DeleteObject" "DeletePermissionsKey"
"DeleteSearchIndex" "DeleteSmallComponents" "DeleteStopwords"
"DeleteWithContents" "DeletionWarning" "DelimitedArray"
"DelimitedSequence" "Delimiter" "DelimiterFlashTime"
"DelimiterMatching" "Delimiters" "DeliveryFunction" "Dendrogram"
"Denominator" "DensityGraphics" "DensityHistogram" "DensityPlot"
"DensityPlot3D" "DependentVariables" "Deploy" "Deployed" "Depth"
"DepthFirstScan" "Derivative" "DerivativeFilter" "DerivativePDETerm"
"DerivedKey" "DescriptorStateSpace" "DesignMatrix"
"DestroyAfterEvaluation" "Det" "DeviceClose" "DeviceConfigure"
"DeviceExecute" "DeviceExecuteAsynchronous" "DeviceObject"
"DeviceOpen" "DeviceOpenQ" "DeviceRead" "DeviceReadBuffer"
"DeviceReadLatest" "DeviceReadList" "DeviceReadTimeSeries" "Devices"
"DeviceStreams" "DeviceWrite" "DeviceWriteBuffer" "DGaussianWavelet"
"DiacriticalPositioning" "Diagonal" "DiagonalizableMatrixQ"
"DiagonalMatrix" "DiagonalMatrixQ" "Dialog" "DialogIndent"
"DialogInput" "DialogLevel" "DialogNotebook" "DialogProlog"
"DialogReturn" "DialogSymbols" "Diamond" "DiamondMatrix"
"DiceDissimilarity" "DictionaryLookup" "DictionaryWordQ"
"DifferenceDelta" "DifferenceOrder" "DifferenceQuotient"
"DifferenceRoot" "DifferenceRootReduce" "Differences" "DifferentialD"
"DifferentialRoot" "DifferentialRootReduce" "DifferentiatorFilter"
"DiffusionPDETerm" "DiggleGatesPointProcess"
"DiggleGrattonPointProcess" "DigitalSignature" "DigitBlock"
"DigitBlockMinimum" "DigitCharacter" "DigitCount" "DigitQ"
"DihedralAngle" "DihedralGroup" "Dilation" "DimensionalCombinations"
"DimensionalMeshComponents" "DimensionReduce"
"DimensionReducerFunction" "DimensionReduction" "Dimensions"
"DiracComb" "DiracDelta" "DirectedEdge" "DirectedEdges"
"DirectedGraph" "DirectedGraphQ" "DirectedInfinity" "DirectHitSearch"
"Direction" "Directive" "Directory" "DirectoryName" "DirectoryQ"
"DirectoryStack" "DirichletBeta" "DirichletCharacter"
"DirichletCondition" "DirichletConvolve" "DirichletDistribution"
"DirichletEta" "DirichletL" "DirichletLambda" "DirichletTransform"
"DirichletWindow" "DisableConsolePrintPacket" "DisableFormatting"
"DiscreteAsymptotic" "DiscreteChirpZTransform" "DiscreteConvolve"
"DiscreteDelta" "DiscreteHadamardTransform" "DiscreteIndicator"
"DiscreteLimit" "DiscreteLQEstimatorGains" "DiscreteLQRegulatorGains"
"DiscreteLyapunovSolve" "DiscreteMarkovProcess" "DiscreteMaxLimit"
"DiscreteMinLimit" "DiscretePlot" "DiscretePlot3D" "DiscreteRatio"
"DiscreteRiccatiSolve" "DiscreteShift" "DiscreteTimeModelQ"
"DiscreteUniformDistribution" "DiscreteVariables"
"DiscreteWaveletData" "DiscreteWaveletPacketTransform"
"DiscreteWaveletTransform" "DiscretizeGraphics" "DiscretizeRegion"
"Discriminant" "DisjointQ" "Disjunction" "Disk" "DiskBox" "DiskMatrix"
"DiskSegment" "Dispatch" "DispatchQ" "DispersionEstimatorFunction"
"Display" "DisplayAllSteps" "DisplayEndPacket"
"DisplayFlushImagePacket" "DisplayForm" "DisplayFunction"
"DisplayPacket" "DisplayRules" "DisplaySetSizePacket" "DisplayString"
"DisplayTemporary" "DisplayWith" "DisplayWithRef"
"DisplayWithVariable" "DistanceFunction" "DistanceMatrix"
"DistanceTransform" "Distribute" "Distributed" "DistributedContexts"
"DistributeDefinitions" "DistributionChart" "DistributionDomain"
"DistributionFitTest" "DistributionParameterAssumptions"
"DistributionParameterQ" "Dithering" "Div" "Divergence" "Divide"
"DivideBy" "Dividers" "DivideSides" "Divisible" "Divisors"
"DivisorSigma" "DivisorSum" "DMSList" "DMSString" "Do" "DockedCells"
"docQueryString" "DocumentationIndexes" "DocumentationIndexMerger"
"DocumentationNotebookIndexer" "DocumentationSpellIndexes"
"DocumentGenerator" "DocumentGeneratorInformation"
"DocumentGeneratorInformationData" "DocumentGenerators"
"DocumentNotebook" "DocumentWeightingRules" "Dodecahedron"
"DomainRegistrationInformation" "DominantColors" "DOSTextFormat" "Dot"
"DotDashed" "DotEqual" "DotLayer" "DotPlusLayer" "Dotted"
"DoubleBracketingBar" "DoubleContourIntegral" "DoubleDownArrow"
"DoubleLeftArrow" "DoubleLeftRightArrow" "DoubleLeftTee"
"DoubleLongLeftArrow" "DoubleLongLeftRightArrow"
"DoubleLongRightArrow" "DoubleRightArrow" "DoubleRightTee"
"DoubleUpArrow" "DoubleUpDownArrow" "DoubleVerticalBar"
"DoublyInfinite" "Down" "DownArrow" "DownArrowBar" "DownArrowUpArrow"
"DownLeftRightVector" "DownLeftTeeVector" "DownLeftVector"
"DownLeftVectorBar" "DownRightTeeVector" "DownRightVector"
"DownRightVectorBar" "Downsample" "DownTee" "DownTeeArrow"
"DownValues" "DragAndDrop" "DrawEdges" "DrawFrontFaces"
"DrawHighlighted" "Drop" "DropoutLayer" "DSolve" "DSolveValue" "Dt"
"DualLinearProgramming" "DualPolyhedron" "DualSystemsModel"
"DumpGet" "DumpSave" "DuplicateFreeQ" "Duration" "Dynamic"
"DynamicBox" "DynamicBoxOptions" "DynamicEvaluationTimeout"
"DynamicGeoGraphics" "DynamicImage" "DynamicLocation" "DynamicModule"
"DynamicModuleBox" "DynamicModuleBoxOptions" "DynamicModuleParent"
"DynamicModuleValues" "DynamicName" "DynamicNamespace"
"DynamicReference" "DynamicSetting" "DynamicUpdating" "DynamicWrapper"
"DynamicWrapperBox" "DynamicWrapperBoxOptions"
))

(defvar xah-wolfram-funs2 nil "List of Wolfram Language symbols. Part of many.")

(setq xah-wolfram-funs2
'(
"E"
"EarthImpactData" "EarthquakeData" "EccentricityCentrality" "Echo"
"EchoEvaluation" "EchoFunction" "EchoLabel" "EchoTiming" "EclipseType"
"EdgeAdd" "EdgeBetweennessCentrality" "EdgeCapacity" "EdgeCapForm"
"EdgeColor" "EdgeConnectivity" "EdgeContract" "EdgeCost" "EdgeCount"
"EdgeCoverQ" "EdgeCycleMatrix" "EdgeDashing" "EdgeDelete" "EdgeDetect"
"EdgeForm" "EdgeIndex" "EdgeJoinForm" "EdgeLabeling" "EdgeLabels"
"EdgeLabelStyle" "EdgeList" "EdgeOpacity" "EdgeQ"
"EdgeRenderingFunction" "EdgeRules" "EdgeShapeFunction" "EdgeStyle"
"EdgeTaggedGraph" "EdgeTaggedGraphQ" "EdgeTags" "EdgeThickness"
"EdgeWeight" "EdgeWeightedGraphQ" "Editable" "EditButtonSettings"
"EditCellTagsSettings" "EditDistance" "EffectiveInterest"
"Eigensystem" "Eigenvalues" "EigenvectorCentrality" "Eigenvectors"
"Element" "ElementData" "ElementwiseLayer" "ElidedForms" "Eliminate"
"EliminationOrder" "Ellipsoid" "EllipticE" "EllipticExp"
"EllipticExpPrime" "EllipticF" "EllipticFilterModel" "EllipticK"
"EllipticLog" "EllipticNomeQ" "EllipticPi"
"EllipticReducedHalfPeriods" "EllipticTheta" "EllipticThetaPrime"
"EmbedCode" "EmbeddedHTML" "EmbeddedService" "EmbeddedSQLEntityClass"
"EmbeddedSQLExpression" "EmbeddingLayer" "EmbeddingObject" "EmitSound"
"EmphasizeSyntaxErrors" "EmpiricalDistribution" "Empty" "EmptyGraphQ"
"EmptyRegion" "EmptySpaceF" "EnableConsolePrintPacket" "Enabled"
"EnablePythonDebug" "Enclose" "Encode" "Encrypt" "EncryptedObject"
"EncryptFile" "End" "EndAdd" "EndDialogPacket"
"EndFrontEndInteractionPacket" "EndOfBuffer" "EndOfFile" "EndOfLine"
"EndOfString" "EndPackage" "EngineEnvironment" "EngineeringForm"
"Enter" "EnterExpressionPacket" "EnterTextPacket" "Entity"
"EntityClass" "EntityClassList" "EntityCopies" "EntityFunction"
"EntityGroup" "EntityInstance" "EntityList" "EntityPrefetch"
"EntityProperties" "EntityProperty" "EntityPropertyClass"
"EntityRegister" "EntityStore" "EntityStores" "EntityType"
"EntityTypeName" "EntityUnregister" "EntityValue" "Entropy"
"EntropyFilter" "Environment" "Epilog" "EpilogFunction" "Equal"
"EqualColumns" "EqualRows" "EqualTilde" "EqualTo" "EquatedTo"
"Equilibrium" "EquirippleFilterKernel" "Equivalent" "Erf" "Erfc"
"Erfi" "ErlangB" "ErlangC" "ErlangDistribution" "Erosion" "ErrorBox"
"ErrorBoxOptions" "ErrorNorm" "ErrorPacket" "ErrorsDialogSettings"
"EscapeRadius" "EstimatedBackground" "EstimatedDistribution"
"EstimatedPointProcess" "EstimatedProcess" "EstimatorGains"
"EstimatorRegulator" "EuclideanDistance" "EulerAngles"
"EulerCharacteristic" "EulerE" "EulerGamma" "EulerianGraphQ"
"EulerMatrix" "EulerPhi" "Evaluatable" "Evaluate" "Evaluated"
"EvaluatePacket" "EvaluateScheduledTask" "EvaluationBox"
"EvaluationCell" "EvaluationCompletionAction" "EvaluationData"
"EvaluationElements" "EvaluationEnvironment" "EvaluationMode"
"EvaluationMonitor" "EvaluationNotebook" "EvaluationObject"
"EvaluationOrder" "EvaluationPrivileges" "EvaluationRateLimit"
"Evaluator" "EvaluatorNames" "EvenQ" "EventData" "EventEvaluator"
"EventHandler" "EventHandlerTag" "EventLabels" "EventSeries"
"ExactBlackmanWindow" "ExactNumberQ" "ExactRootIsolation"
"ExampleData" "Except" "ExcludedContexts" "ExcludedForms"
"ExcludedLines" "ExcludedPhysicalQuantities" "ExcludePods"
"Exclusions" "ExclusionsStyle" "Exists" "Exit" "ExitDialog"
"ExoplanetData" "Exp" "Expand" "ExpandAll" "ExpandDenominator"
"ExpandFileName" "ExpandNumerator" "Expectation" "ExpectationE"
"ExpectedValue" "ExpGammaDistribution" "ExpIntegralE" "ExpIntegralEi"
"ExpirationDate" "Exponent" "ExponentFunction"
"ExponentialDistribution" "ExponentialFamily"
"ExponentialGeneratingFunction" "ExponentialMovingAverage"
"ExponentialPowerDistribution" "ExponentPosition" "ExponentStep"
"Export" "ExportAutoReplacements" "ExportByteArray" "ExportForm"
"ExportPacket" "ExportSearchResults" "ExportString" "Expression"
"ExpressionCell" "ExpressionGraph" "ExpressionPacket" "ExpressionUUID"
"ExpToTrig" "ExtendedEntityClass" "ExtendedGCD" "Extension"
"ExtentElementFunction" "ExtentMarkers" "ExtentSize" "ExternalBundle"
"ExternalCall" "ExternalDataCharacterEncoding" "ExternalEvaluate"
"ExternalFunction" "ExternalFunctionName" "ExternalIdentifier"
"ExternalObject" "ExternalOptions" "ExternalSessionObject"
"ExternalSessions" "ExternalStorageBase" "ExternalStorageDownload"
"ExternalStorageGet" "ExternalStorageObject" "ExternalStoragePut"
"ExternalStorageUpload" "ExternalTypeSignature" "ExternalValue"
"Extract" "ExtractArchive" "ExtractLayer" "ExtractPacletArchive"
"ExtremeValueDistribution" "FaceAlign" "FaceForm" "FaceGrids"
"FaceGridsStyle" "FaceRecognize" "FacialFeatures" "Factor"
"FactorComplete" "Factorial" "Factorial2" "FactorialMoment"
"FactorialMomentGeneratingFunction" "FactorialPower" "FactorInteger"
"FactorList" "FactorSquareFree" "FactorSquareFreeList" "FactorTerms"
"FactorTermsList" "Fail" "Failure" "FailureAction"
"FailureDistribution" "FailureQ" "False" "FareySequence"
"FARIMAProcess" "FeatureDistance" "FeatureExtract" "FeatureExtraction"
"FeatureExtractor" "FeatureExtractorFunction" "FeatureNames"
"FeatureNearest" "FeatureSpacePlot" "FeatureSpacePlot3D"
"FeatureTypes" "FEDisableConsolePrintPacket" "FeedbackLinearize"
"FeedbackSector" "FeedbackSectorStyle" "FeedbackType"
"FEEnableConsolePrintPacket" "FetalGrowthData"
"FetchReferencedFunctions" "Fibonacci" "Fibonorial"
"FieldCompletionFunction" "FieldHint" "FieldHintStyle" "FieldMasked"
"FieldSize" "File" "FileBaseName" "FileByteCount" "FileConvert"
"FileDate" "FileExistsQ" "FileExtension" "FileFormat" "FileFormatQ"
"FileHandler" "FileHash" "FileInformation" "FileNameDepth"
"FileNameDialogSettings" "FileNameDrop" "FileNameForms" "FileNameJoin"
"FileNames" "FileNameSetter" "FileNameSplit" "FileNameTake"
"FilePrint" "FileSize" "FileSystemMap" "FileSystemScan" "FileTemplate"
"FileTemplateApply" "FileType" "FilledCurve" "FilledCurveBox"
"FilledCurveBoxOptions" "Filling" "FillingStyle" "FillingTransform"
"FilteredEntityClass" "FilterRules" "FinancialBond" "FinancialData"
"FinancialDerivative" "FinancialIndicator" "Find" "FindAnomalies"
"FindArgMax" "FindArgMin" "FindAvailableApplications"
"FindAvailableDocumentation" "FindChannels" "FindClique"
"FindClusters" "FindCookies" "FindCurvePath" "FindCycle" "FindDevices"
"FindDistribution" "FindDistributionParameters" "FindDivisions"
"FindEdgeCover" "FindEdgeCut" "FindEdgeIndependentPaths"
"FindEquationalProof" "FindEulerianCycle" "FindExternalEvaluators"
"FindFaces" "FindFile" "FindFit" "FindFormula" "FindFundamentalCycles"
"FindGeneratingFunction" "FindGeoLocation" "FindGeometricConjectures"
"FindGeometricTransform" "FindGraphCommunities" "FindGraphIsomorphism"
"FindGraphPartition" "FindHamiltonianCycle" "FindHamiltonianPath"
"FindHiddenMarkovStates" "FindImageText" "FindIndependentEdgeSet"
"FindIndependentVertexSet" "FindInstance" "FindIntegerNullVector"
"FindKClan" "FindKClique" "FindKClub" "FindKPlex" "FindLibrary"
"FindLinearRecurrence" "FindList" "FindMatchingColor" "FindMaximum"
"FindMaximumCut" "FindMaximumFlow" "FindMaxValue" "FindMeshDefects"
"FindMinimum" "FindMinimumCostFlow" "FindMinimumCut" "FindMinValue"
"FindMoleculeSubstructure" "FindPath" "FindPeaks" "FindPermutation"
"FindPointProcessParameters" "FindPostmanTour" "FindProcessParameters"
"FindRepeat" "FindRoot" "FindSequenceFunction" "FindSettings"
"FindShortestPath" "FindShortestTour" "FindSpanningTree"
"FindSystemModelEquilibrium" "FindTextualAnswer" "FindThreshold"
"FindTransientRepeat" "FindVertexCover" "FindVertexCut"
"FindVertexIndependentPaths" "Fine" "FinishDynamic"
"FiniteAbelianGroupCount" "FiniteGroupCount" "FiniteGroupData" "First"
"FirstCase" "FirstPassageTimeDistribution" "FirstPosition"
"FischerGroupFi22" "FischerGroupFi23" "FischerGroupFi24Prime"
"FisherHypergeometricDistribution" "FisherRatioTest"
"FisherZDistribution" "Fit" "FitAll" "FitRegularization" "FittedModel"
"FixedOrder" "FixedPoint" "FixedPointList" "FlashSelection" "Flat"
"Flatten" "FlattenAt" "FlattenLayer" "FlatTopWindow" "FlipView"
"Floor" "FlowPolynomial" "FlushPrintOutputPacket" "Fold" "FoldList"
"FoldPair" "FoldPairList" "FoldWhile" "FoldWhileList"
"FollowRedirects" "Font" "FontColor" "FontFamily" "FontForm"
"FontName" "FontOpacity" "FontPostScriptName" "FontProperties"
"FontReencoding" "FontSize" "FontSlant" "FontSubstitutions"
"FontTracking" "FontVariations" "FontWeight" "For" "ForAll"
"ForceVersionInstall" "Format" "FormatRules" "FormatType"
"FormatTypeAutoConvert" "FormatValues" "FormBox" "FormBoxOptions"
"FormControl" "FormFunction" "FormLayoutFunction" "FormObject"
"FormPage" "FormProtectionMethod" "FormTheme" "FormulaData"
"FormulaLookup" "FortranForm" "Forward" "ForwardBackward"
"ForwardCloudCredentials" "Fourier" "FourierCoefficient"
"FourierCosCoefficient" "FourierCosSeries" "FourierCosTransform"
"FourierDCT" "FourierDCTFilter" "FourierDCTMatrix" "FourierDST"
"FourierDSTMatrix" "FourierMatrix" "FourierParameters"
"FourierSequenceTransform" "FourierSeries" "FourierSinCoefficient"
"FourierSinSeries" "FourierSinTransform" "FourierTransform"
"FourierTrigSeries" "FractionalBrownianMotionProcess"
"FractionalGaussianNoiseProcess" "FractionalPart" "FractionBox"
"FractionBoxOptions" "FractionLine" "Frame" "FrameBox"
"FrameBoxOptions" "Framed" "FrameInset" "FrameLabel" "Frameless"
"FrameMargins" "FrameRate" "FrameStyle" "FrameTicks" "FrameTicksStyle"
"FRatioDistribution" "FrechetDistribution" "FreeQ"
"FrenetSerretSystem" "FrequencySamplingFilterKernel" "FresnelC"
"FresnelF" "FresnelG" "FresnelS" "Friday" "FrobeniusNumber"
"FrobeniusSolve" "FromAbsoluteTime" "FromCharacterCode"
"FromCoefficientRules" "FromContinuedFraction" "FromDate" "FromDigits"
"FromDMS" "FromEntity" "FromJulianDate" "FromLetterNumber"
"FromPolarCoordinates" "FromRomanNumeral" "FromSphericalCoordinates"
"FromUnixTime" "Front" "FrontEndDynamicExpression"
"FrontEndEventActions" "FrontEndExecute" "FrontEndObject"
"FrontEndResource" "FrontEndResourceString" "FrontEndStackSize"
"FrontEndToken" "FrontEndTokenExecute" "FrontEndValueCache"
"FrontEndVersion" "FrontFaceColor" "FrontFaceOpacity" "Full"
"FullAxes" "FullDefinition" "FullForm" "FullGraphics"
"FullInformationOutputRegulator" "FullOptions" "FullRegion"
"FullScreenArea" "FullSimplify" "Function" "FunctionAnalytic"
"FunctionBijective" "FunctionCompile" "FunctionCompileExport"
"FunctionCompileExportByteArray" "FunctionCompileExportLibrary"
"FunctionCompileExportString" "FunctionContinuous" "FunctionConvexity"
"FunctionDiscontinuities" "FunctionDomain" "FunctionExpand"
"FunctionInjective" "FunctionInterpolation" "FunctionLayer"
"FunctionMeromorphic" "FunctionMonotonicity" "FunctionPeriod"
"FunctionRange" "FunctionSign" "FunctionSingularities" "FunctionSpace"
"FunctionSurjective" "FussellVeselyImportance" "GaborFilter"
"GaborMatrix" "GaborWavelet" "GainMargins" "GainPhaseMargins"
"GalaxyData" "GalleryView" "Gamma" "GammaDistribution"
"GammaRegularized" "GapPenalty" "GARCHProcess" "GatedRecurrentLayer"
"Gather" "GatherBy" "GaugeFaceElementFunction" "GaugeFaceStyle"
"GaugeFrameElementFunction" "GaugeFrameSize" "GaugeFrameStyle"
"GaugeLabels" "GaugeMarkers" "GaugeStyle" "GaussianFilter"
"GaussianIntegers" "GaussianMatrix"
"GaussianOrthogonalMatrixDistribution"
"GaussianSymplecticMatrixDistribution"
"GaussianUnitaryMatrixDistribution" "GaussianWindow" "GCD"
"GegenbauerC" "General" "GeneralizedLinearModelFit"
"GenerateAsymmetricKeyPair" "GenerateConditions"
"GeneratedAssetLocation" "GeneratedCell" "GeneratedDocumentBinding"
"GenerateDerivedKey" "GenerateDigitalSignature" "GenerateDocument"
"GeneratedParameters" "GeneratedQuantityMagnitudes"
"GenerateFileSignature" "GenerateHTTPResponse"
"GenerateSecuredAuthenticationKey" "GenerateSymmetricKey"
"GeneratingFunction" "GeneratorDescription" "GeneratorHistoryLength"
"GeneratorOutputType" "Generic" "GenericCylindricalDecomposition"
"GenomeData" "GenomeLookup" "GeoAntipode" "GeoArea" "GeoArraySize"
"GeoBackground" "GeoBoundary" "GeoBoundingBox" "GeoBounds"
"GeoBoundsRegion" "GeoBoundsRegionBoundary" "GeoBubbleChart"
"GeoCenter" "GeoCircle" "GeoContourPlot" "GeoDensityPlot"
"GeodesicClosing" "GeodesicDilation" "GeodesicErosion"
"GeodesicOpening" "GeoDestination" "GeodesyData" "GeoDirection"
"GeoDisk" "GeoDisplacement" "GeoDistance" "GeoDistanceList"
"GeoElevationData" "GeoEntities" "GeoGraphics" "GeogravityModelData"
"GeoGridDirectionDifference" "GeoGridLines" "GeoGridLinesStyle"
"GeoGridPosition" "GeoGridRange" "GeoGridRangePadding"
"GeoGridUnitArea" "GeoGridUnitDistance" "GeoGridVector" "GeoGroup"
"GeoHemisphere" "GeoHemisphereBoundary" "GeoHistogram" "GeoIdentify"
"GeoImage" "GeoLabels" "GeoLength" "GeoListPlot" "GeoLocation"
"GeologicalPeriodData" "GeomagneticModelData" "GeoMarker"
"GeometricAssertion" "GeometricBrownianMotionProcess"
"GeometricDistribution" "GeometricMean" "GeometricMeanFilter"
"GeometricOptimization" "GeometricScene" "GeometricStep"
"GeometricTransformation" "GeometricTransformation3DBox"
"GeometricTransformation3DBoxOptions" "GeometricTransformationBox"
"GeometricTransformationBoxOptions" "GeoModel" "GeoNearest" "GeoPath"
"GeoPolygon" "GeoPosition" "GeoPositionENU" "GeoPositionXYZ"
"GeoProjection" "GeoProjectionData" "GeoRange" "GeoRangePadding"
"GeoRegionValuePlot" "GeoResolution" "GeoScaleBar" "GeoServer"
"GeoSmoothHistogram" "GeoStreamPlot" "GeoStyling"
"GeoStylingImageFunction" "GeoVariant" "GeoVector" "GeoVectorENU"
"GeoVectorPlot" "GeoVectorXYZ" "GeoVisibleRegion"
"GeoVisibleRegionBoundary" "GeoWithinQ" "GeoZoomLevel"
"GestureHandler" "GestureHandlerTag" "Get" "GetBoundingBoxSizePacket"
"GetContext" "GetEnvironment" "GetFileName"
"GetFrontEndOptionsDataPacket" "GetLinebreakInformationPacket"
"GetMenusPacket" "GetPageBreakInformationPacket" "GibbsPointProcess"
"Glaisher" "GlobalClusteringCoefficient" "GlobalPreferences"
"GlobalSession" "Glow" "GoldenAngle" "GoldenRatio"
"GompertzMakehamDistribution" "GoochShading" "GoodmanKruskalGamma"
"GoodmanKruskalGammaTest" "Goto" "Grad" "Gradient" "GradientFilter"
"GradientOrientationFilter" "GrammarApply" "GrammarRules"
"GrammarToken" "Graph" "Graph3D" "GraphAssortativity"
"GraphAutomorphismGroup" "GraphCenter" "GraphComplement" "GraphData"
"GraphDensity" "GraphDiameter" "GraphDifference" "GraphDisjointUnion"
"GraphDistance" "GraphDistanceMatrix" "GraphElementData"
"GraphEmbedding" "GraphHighlight" "GraphHighlightStyle" "GraphHub"
"Graphics" "Graphics3D" "Graphics3DBox" "Graphics3DBoxOptions"
"GraphicsBaseline" "GraphicsBox" "GraphicsBoxOptions"
"GraphicsColor" "GraphicsColumn" "GraphicsComplex"
"GraphicsComplex3DBox" "GraphicsComplex3DBoxOptions"
"GraphicsComplexBox" "GraphicsComplexBoxOptions" "GraphicsContents"
"GraphicsData" "GraphicsGrid" "GraphicsGridBox" "GraphicsGroup"
"GraphicsGroup3DBox" "GraphicsGroup3DBoxOptions" "GraphicsGroupBox"
"GraphicsGroupBoxOptions" "GraphicsGrouping" "GraphicsHighlightColor"
"GraphicsRow" "GraphicsSpacing" "GraphicsStyle" "GraphIntersection"
"GraphLayout" "GraphLinkEfficiency" "GraphPeriphery" "GraphPlot"
"GraphPlot3D" "GraphPower" "GraphPropertyDistribution" "GraphQ"
"GraphRadius" "GraphReciprocity" "GraphRoot" "GraphStyle" "GraphUnion"
"Gray" "GrayLevel" "Greater" "GreaterEqual" "GreaterEqualLess"
"GreaterEqualThan" "GreaterFullEqual" "GreaterGreater" "GreaterLess"
"GreaterSlantEqual" "GreaterThan" "GreaterTilde" "Green"
"GreenFunction" "Grid" "GridBaseline" "GridBox" "GridBoxAlignment"
"GridBoxBackground" "GridBoxDividers" "GridBoxFrame" "GridBoxItemSize"
"GridBoxItemStyle" "GridBoxOptions" "GridBoxSpacings"
"GridCreationSettings" "GridDefaultElement" "GridElementStyleOptions"
"GridFrame" "GridFrameMargins" "GridGraph" "GridLines"
"GridLinesStyle" "GroebnerBasis" "GroupActionBase" "GroupBy"
"GroupCentralizer" "GroupElementFromWord" "GroupElementPosition"
"GroupElementQ" "GroupElements" "GroupElementToWord" "GroupGenerators"
"Groupings" "GroupMultiplicationTable" "GroupOrbits" "GroupOrder"
"GroupPageBreakWithin" "GroupSetwiseStabilizer" "GroupStabilizer"
"GroupStabilizerChain" "GroupTogetherGrouping"
"GroupTogetherNestedGrouping" "GrowCutComponents" "Gudermannian"
"GuidedFilter" "GumbelDistribution" "HaarWavelet" "HadamardMatrix"
"HalfLine" "HalfNormalDistribution" "HalfPlane" "HalfSpace"
"HalftoneShading" "HamiltonianGraphQ" "HammingDistance"
"HammingWindow" "HandlerFunctions" "HandlerFunctionsKeys" "HankelH1"
"HankelH2" "HankelMatrix" "HankelTransform" "HannPoissonWindow"
"HannWindow" "HaradaNortonGroupHN" "HararyGraph"
"HardcorePointProcess" "HarmonicMean" "HarmonicMeanFilter"
"HarmonicNumber" "Hash" "HatchFilling" "HatchShading" "Haversine"
"HazardFunction" "Head" "HeadCompose" "HeaderAlignment"
"HeaderBackground" "HeaderDisplayFunction" "HeaderLines" "Headers"
"HeaderSize" "HeaderStyle" "Heads" "HeatFluxValue"
"HeatInsulationValue" "HeatOutflowValue" "HeatRadiationValue"
"HeatSymmetryValue" "HeatTemperatureCondition"
"HeatTransferPDEComponent" "HeatTransferValue" "HeavisideLambda"
"HeavisidePi" "HeavisideTheta" "HeldGroupHe" "HeldPart"
"HelmholtzPDEComponent" "HelpBrowserLookup" "HelpBrowserNotebook"
"HelpBrowserSettings" "Here" "HermiteDecomposition" "HermiteH"
"Hermitian" "HermitianMatrixQ" "HessenbergDecomposition" "Hessian"
"HeunB" "HeunBPrime" "HeunC" "HeunCPrime" "HeunD" "HeunDPrime" "HeunG"
"HeunGPrime" "HeunT" "HeunTPrime" "HexadecimalCharacter" "Hexahedron"
"HexahedronBox" "HexahedronBoxOptions" "HiddenItems"
"HiddenMarkovProcess" "HiddenSurface" "Highlighted" "HighlightGraph"
"HighlightImage" "HighlightMesh" "HighpassFilter" "HigmanSimsGroupHS"
"HilbertCurve" "HilbertFilter" "HilbertMatrix" "Histogram"
"Histogram3D" "HistogramDistribution" "HistogramList"
"HistogramPointDensity" "HistogramTransform"
"HistogramTransformInterpolation" "HistoricalPeriodData"
"HitMissTransform" "HITSCentrality" "HjorthDistribution" "HodgeDual"
"HoeffdingD" "HoeffdingDTest" "Hold" "HoldAll" "HoldAllComplete"
"HoldComplete" "HoldFirst" "HoldForm" "HoldPattern" "HoldRest"
"HolidayCalendar" "HomeDirectory" "HomePage" "Horizontal"
"HorizontalForm" "HorizontalGauge" "HorizontalScrollPosition"
"HornerForm" "HostLookup" "HotellingTSquareDistribution"
"HoytDistribution" "HTMLSave" "HTTPErrorResponse" "HTTPRedirect"
"HTTPRequest" "HTTPRequestData" "HTTPResponse" "Hue" "HumanGrowthData"
"HumpDownHump" "HumpEqual" "HurwitzLerchPhi" "HurwitzZeta"
"HyperbolicDistribution" "HypercubeGraph"
"HyperexponentialDistribution" "Hyperfactorial" "Hypergeometric0F1"
"Hypergeometric0F1Regularized" "Hypergeometric1F1"
"Hypergeometric1F1Regularized" "Hypergeometric2F1"
"Hypergeometric2F1Regularized" "HypergeometricDistribution"
"HypergeometricPFQ" "HypergeometricPFQRegularized" "HypergeometricU"
"Hyperlink" "HyperlinkAction" "HyperlinkCreationSettings" "Hyperplane"
"Hyphenation" "HyphenationOptions" "HypoexponentialDistribution"
"HypothesisTestData"
)
)

(defvar xah-wolfram-funs2-5 nil "List of Wolfram Language symbols. Part of many.")
(setq xah-wolfram-funs2-5 '("I" "IconData" "Iconize" "IconizedObject"
"IconRules" "Icosahedron" "Identity" "IdentityMatrix" "If"
"IgnoreCase" "IgnoreDiacritics" "IgnorePunctuation" "IgnoreSpellCheck"
"IgnoringInactive" "Im" "Image" "Image3D" "Image3DProjection"
"Image3DSlices" "ImageAccumulate" "ImageAdd" "ImageAdjust"
"ImageAlign" "ImageApply" "ImageApplyIndexed" "ImageAspectRatio"
"ImageAssemble" "ImageAugmentationLayer" "ImageBoundingBoxes"
"ImageCache" "ImageCacheValid" "ImageCapture" "ImageCaptureFunction"
"ImageCases" "ImageChannels" "ImageClip" "ImageCollage"
"ImageColorSpace" "ImageCompose" "ImageContainsQ" "ImageContents"
"ImageConvolve" "ImageCooccurrence" "ImageCorners" "ImageCorrelate"
"ImageCorrespondingPoints" "ImageCrop" "ImageData" "ImageDeconvolve"
"ImageDemosaic" "ImageDifference" "ImageDimensions"
"ImageDisplacements" "ImageDistance" "ImageEffect"
"ImageExposureCombine" "ImageFeatureTrack" "ImageFileApply"
"ImageFileFilter" "ImageFileScan" "ImageFilter" "ImageFocusCombine"
"ImageForestingComponents" "ImageFormattingWidth"
"ImageForwardTransformation" "ImageGraphics" "ImageHistogram"
"ImageIdentify" "ImageInstanceQ" "ImageKeypoints" "ImageLabels"
"ImageLegends" "ImageLevels" "ImageLines" "ImageMargins" "ImageMarker"
"ImageMarkers" "ImageMeasurements" "ImageMesh" "ImageMultiply"
"ImageOffset" "ImagePad" "ImagePadding" "ImagePartition"
"ImagePeriodogram" "ImagePerspectiveTransformation" "ImagePosition"
"ImagePreviewFunction" "ImagePyramid" "ImagePyramidApply" "ImageQ"
"ImageRangeCache" "ImageRecolor" "ImageReflect" "ImageRegion"
"ImageResize" "ImageResolution" "ImageRestyle" "ImageRotate"
"ImageRotated" "ImageSaliencyFilter" "ImageScaled" "ImageScan"
"ImageSize" "ImageSizeAction" "ImageSizeCache" "ImageSizeMultipliers"
"ImageSizeRaw" "ImageSubtract" "ImageTake" "ImageTransformation"
"ImageTrim" "ImageType" "ImageValue" "ImageValuePositions"
"ImageVectorscopePlot" "ImageWaveformPlot" "ImagingDevice"
"ImplicitRegion" "Implies" "Import" "ImportAutoReplacements"
"ImportByteArray" "ImportedObject" "ImportOptions" "ImportString"
"ImprovementImportance" "In" "Inactivate" "Inactive" "IncidenceGraph"
"IncidenceList" "IncidenceMatrix" "IncludeAromaticBonds"
"IncludeConstantBasis" "IncludedContexts" "IncludeDefinitions"
"IncludeDirectories" "IncludeFileExtension" "IncludeGeneratorTasks"
"IncludeHydrogens" "IncludeInflections" "IncludeMetaInformation"
"IncludePods" "IncludeQuantities" "IncludeRelatedTables"
"IncludeSingularTerm" "IncludeWindowTimes" "Increment"
"IndefiniteMatrixQ" "Indent" "IndentingNewlineSpacings"
"IndentMaxFraction" "IndependenceTest" "IndependentEdgeSetQ"
"IndependentPhysicalQuantity" "IndependentUnit"
"IndependentUnitDimension" "IndependentVertexSetQ" "Indeterminate"
"IndeterminateThreshold" "IndexCreationOptions" "Indexed"
"IndexEdgeTaggedGraph" "IndexGraph" "IndexTag" "Inequality"
"InexactNumberQ" "InexactNumbers" "InfiniteFuture" "InfiniteLine"
"InfinitePast" "InfinitePlane" "Infinity" "Infix" "InflationAdjust"
"InflationMethod" "Information" "InformationData"
"InformationDataGrid" "Inherited" "InheritScope"
"InhomogeneousPoissonPointProcess" "InhomogeneousPoissonProcess"
"InitialEvaluationHistory" "Initialization" "InitializationCell"
"InitializationCellEvaluation" "InitializationCellWarning"
"InitializationObjects" "InitializationValue" "Initialize"
"InitializeDocumentationSearch" "InitialSeeding"
"InlineCounterAssignments" "InlineCounterIncrements" "InlineRules"
"Inner" "InnerPolygon" "InnerPolyhedron" "Inpaint" "Input"
"InputAliases" "InputAssumptions" "InputAutoReplacements" "InputField"
"InputFieldBox" "InputFieldBoxOptions" "InputForm" "InputGrouping"
"InputNamePacket" "InputNotebook" "InputPacket" "InputPorts"
"InputSettings" "InputStream" "InputString" "InputStringPacket"
"InputToBoxFormPacket" "Insert" "InsertionFunction"
"InsertionPointObject" "InsertLinebreaks" "InsertResults" "Inset"
"Inset3DBox" "Inset3DBoxOptions" "InsetBox" "InsetBoxOptions"
"Insphere" "Install" "InstallService" "InstanceNormalizationLayer"
"InString" "Integer" "IntegerDigits" "IntegerExponent" "IntegerLength"
"IntegerName" "IntegerPart" "IntegerPartitions" "IntegerQ"
"IntegerReverse" "Integers" "IntegerString" "Integral" "Integrate"
"Interactive" "InteractiveTradingChart" "Interlaced" "Interleaving"
"InternallyBalancedDecomposition" "InterpolatingFunction"
"InterpolatingPolynomial" "Interpolation" "InterpolationOrder"
"InterpolationPoints" "InterpolationPrecision" "Interpretation"
"InterpretationBox" "InterpretationBoxOptions"
"InterpretationFunction" "Interpreter" "InterpretTemplate"
"InterquartileRange" "Interrupt" "InterruptSettings"
"IntersectedEntityClass" "IntersectingQ" "Intersection" "Interval"
"IntervalIntersection" "IntervalMarkers" "IntervalMarkersStyle"
"IntervalMemberQ" "IntervalSlider" "IntervalUnion" "Into" "Inverse"
"InverseBetaRegularized" "InverseCDF" "InverseChiSquareDistribution"
"InverseContinuousWaveletTransform" "InverseDistanceTransform"
"InverseEllipticNomeQ" "InverseErf" "InverseErfc" "InverseFourier"
"InverseFourierCosTransform" "InverseFourierSequenceTransform"
"InverseFourierSinTransform" "InverseFourierTransform"
"InverseFunction" "InverseFunctions" "InverseGammaDistribution"
"InverseGammaRegularized" "InverseGaussianDistribution"
"InverseGudermannian" "InverseHankelTransform" "InverseHaversine"
"InverseImagePyramid" "InverseJacobiCD" "InverseJacobiCN"
"InverseJacobiCS" "InverseJacobiDC" "InverseJacobiDN"
"InverseJacobiDS" "InverseJacobiNC" "InverseJacobiND"
"InverseJacobiNS" "InverseJacobiSC" "InverseJacobiSD"
"InverseJacobiSN" "InverseLaplaceTransform" "InverseMellinTransform"
"InversePermutation" "InverseRadon" "InverseRadonTransform"
"InverseSeries" "InverseShortTimeFourier" "InverseSpectrogram"
"InverseSurvivalFunction" "InverseTransformedRegion"
"InverseWaveletTransform" "InverseWeierstrassP"
"InverseWishartMatrixDistribution" "InverseZTransform" "Invisible"
"InvisibleApplication" "InvisibleTimes" "IPAddress"
"IrreduciblePolynomialQ" "IslandData" "IsolatingInterval"
"IsomorphicGraphQ" "IsotopeData" "Italic" "Item" "ItemAspectRatio"
"ItemBox" "ItemBoxOptions" "ItemDisplayFunction" "ItemSize"
"ItemStyle" "ItoProcess" "JaccardDissimilarity" "JacobiAmplitude"
"Jacobian" "JacobiCD" "JacobiCN" "JacobiCS" "JacobiDC" "JacobiDN"
"JacobiDS" "JacobiEpsilon" "JacobiNC" "JacobiND" "JacobiNS" "JacobiP"
"JacobiSC" "JacobiSD" "JacobiSN" "JacobiSymbol" "JacobiZeta"
"JacobiZN" "JankoGroupJ1" "JankoGroupJ2" "JankoGroupJ3" "JankoGroupJ4"
"JarqueBeraALMTest" "JohnsonDistribution" "Join" "JoinAcross" "Joined"
"JoinedCurve" "JoinedCurveBox" "JoinedCurveBoxOptions" "JoinForm"
"JordanDecomposition" "JordanModelDecomposition" "JulianDate"
"JuliaSetBoettcher" "JuliaSetIterationCount" "JuliaSetPlot"
"JuliaSetPoints" "K" "KagiChart" "KaiserBesselWindow" "KaiserWindow"
"KalmanEstimator" "KalmanFilter" "KarhunenLoeveDecomposition"
"KaryTree" "KatzCentrality" "KCoreComponents" "KDistribution"
"KEdgeConnectedComponents" "KEdgeConnectedGraphQ"
"KeepExistingVersion" "KelvinBei" "KelvinBer" "KelvinKei" "KelvinKer"
"KendallTau" "KendallTauTest" "KernelExecute" "KernelFunction"
"KernelMixtureDistribution" "KernelObject" "Kernels" "Ket" "Key"
"KeyCollisionFunction" "KeyComplement" "KeyDrop" "KeyDropFrom"
"KeyExistsQ" "KeyFreeQ" "KeyIntersection" "KeyMap" "KeyMemberQ"
"KeypointStrength" "Keys" "KeySelect" "KeySort" "KeySortBy" "KeyTake"
"KeyUnion" "KeyValueMap" "KeyValuePattern" "Khinchin" "KillProcess"
"KirchhoffGraph" "KirchhoffMatrix" "KleinInvariantJ" "KnapsackSolve"
"KnightTourGraph" "KnotData" "KnownUnitQ" "KochCurve"
"KolmogorovSmirnovTest" "KroneckerDelta" "KroneckerModelDecomposition"
"KroneckerProduct" "KroneckerSymbol" "KuiperTest"
"KumaraswamyDistribution" "Kurtosis" "KuwaharaFilter"
"KVertexConnectedComponents" "KVertexConnectedGraphQ" "LABColor"
"Label" "Labeled" "LabeledSlider" "LabelingFunction" "LabelingSize"
"LabelStyle" "LabelVisibility" "LaguerreL" "LakeData"
"LambdaComponents" "LambertW" "LameC" "LameCPrime" "LameEigenvalueA"
"LameEigenvalueB" "LameS" "LameSPrime" "LaminaData" "LanczosWindow"
"LandauDistribution" "Language" "LanguageCategory" "LanguageData"
"LanguageIdentify" "LanguageOptions" "LaplaceDistribution"
"LaplaceTransform" "Laplacian" "LaplacianFilter"
"LaplacianGaussianFilter" "LaplacianPDETerm" "Large" "Larger" "Last"
"Latitude" "LatitudeLongitude" "LatticeData" "LatticeReduce" "Launch"
"LaunchKernels" "LayeredGraphPlot" "LayerSizeFunction"
"LayoutInformation" "LCHColor" "LCM" "LeaderSize" "LeafCount"
"LeapVariant" "LeapYearQ" "LearnDistribution" "LearnedDistribution"
"LearningRate" "LearningRateMultipliers" "LeastSquares"
"LeastSquaresFilterKernel" "Left" "LeftArrow" "LeftArrowBar"
"LeftArrowRightArrow" "LeftDownTeeVector" "LeftDownVector"
"LeftDownVectorBar" "LeftRightArrow" "LeftRightVector" "LeftTee"
"LeftTeeArrow" "LeftTeeVector" "LeftTriangle" "LeftTriangleBar"
"LeftTriangleEqual" "LeftUpDownVector" "LeftUpTeeVector"
"LeftUpVector" "LeftUpVectorBar" "LeftVector" "LeftVectorBar"
"LegendAppearance" "Legended" "LegendFunction" "LegendLabel"
"LegendLayout" "LegendMargins" "LegendMarkers" "LegendMarkerSize"
"LegendreP" "LegendreQ" "LegendreType" "Length" "LengthWhile"
"LerchPhi" "Less" "LessEqual" "LessEqualGreater" "LessEqualThan"
"LessFullEqual" "LessGreater" "LessLess" "LessSlantEqual" "LessThan"
"LessTilde" "LetterCharacter" "LetterCounts" "LetterNumber" "LetterQ"
"Level" "LeveneTest" "LeviCivitaTensor" "LevyDistribution"
"Lexicographic" "LibraryDataType" "LibraryFunction"
"LibraryFunctionError" "LibraryFunctionInformation"
"LibraryFunctionLoad" "LibraryFunctionUnload" "LibraryLoad"
"LibraryUnload" "LicenseEntitlementObject" "LicenseEntitlements"
"LicenseID" "LicensingSettings" "LiftingFilterData"
"LiftingWaveletTransform" "LightBlue" "LightBrown" "LightCyan"
"Lighter" "LightGray" "LightGreen" "Lighting" "LightingAngle"
"LightMagenta" "LightOrange" "LightPink" "LightPurple" "LightRed"
"LightSources" "LightYellow" "Likelihood" "Limit" "LimitsPositioning"
"LimitsPositioningTokens" "LindleyDistribution" "Line" "Line3DBox"
"Line3DBoxOptions" "LinearFilter" "LinearFractionalOptimization"
"LinearFractionalTransform" "LinearGradientFilling"
"LinearGradientImage" "LinearizingTransformationData" "LinearLayer"
"LinearModelFit" "LinearOffsetFunction" "LinearOptimization"
"LinearProgramming" "LinearRecurrence" "LinearSolve"
"LinearSolveFunction" "LineBox" "LineBoxOptions" "LineBreak"
"LinebreakAdjustments" "LineBreakChart" "LinebreakSemicolonWeighting"
"LineBreakWithin" "LineColor" "LineGraph" "LineIndent"
"LineIndentMaxFraction" "LineIntegralConvolutionPlot"
"LineIntegralConvolutionScale" "LineLegend" "LineOpacity"
"LineSpacing" "LineWrapParts" "LinkActivate" "LinkClose" "LinkConnect"
"LinkConnectedQ" "LinkCreate" "LinkError" "LinkFlush" "LinkFunction"
"LinkHost" "LinkInterrupt" "LinkLaunch" "LinkMode" "LinkObject"
"LinkOpen" "LinkOptions" "LinkPatterns" "LinkProtocol"
"LinkRankCentrality" "LinkRead" "LinkReadHeld" "LinkReadyQ" "Links"
"LinkService" "LinkWrite" "LinkWriteHeld" "LiouvilleLambda" "List"
"Listable" "ListAnimate" "ListContourPlot" "ListContourPlot3D"
"ListConvolve" "ListCorrelate" "ListCurvePathPlot" "ListDeconvolve"
"ListDensityPlot" "ListDensityPlot3D" "Listen" "ListFormat"
"ListFourierSequenceTransform" "ListInterpolation"
"ListLineIntegralConvolutionPlot" "ListLinePlot" "ListLogLinearPlot"
"ListLogLogPlot" "ListLogPlot" "ListPicker" "ListPickerBox"
"ListPickerBoxBackground" "ListPickerBoxOptions" "ListPlay" "ListPlot"
"ListPlot3D" "ListPointPlot3D" "ListPolarPlot" "ListQ"
"ListSliceContourPlot3D" "ListSliceDensityPlot3D"
"ListSliceVectorPlot3D" "ListStepPlot" "ListStreamDensityPlot"
"ListStreamPlot" "ListSurfacePlot3D" "ListVectorDensityPlot"
"ListVectorPlot" "ListVectorPlot3D" "ListZTransform" "Literal"
"LiteralSearch" "LoadTriangle" "LocalAdaptiveBinarize" "LocalCache"
"LocalClusteringCoefficient" "LocalizeDefinitions" "LocalizeVariables"
"LocalObject" "LocalObjects" "LocalResponseNormalizationLayer"
"LocalSubmit" "LocalSymbol" "LocalTime" "LocalTimeZone"
"LocationEquivalenceTest" "LocationTest" "Locator" "LocatorAutoCreate"
"LocatorBox" "LocatorBoxOptions" "LocatorCentering" "LocatorPane"
"LocatorPaneBox" "LocatorPaneBoxOptions" "LocatorRegion" "Locked"
"Log" "Log10" "Log2" "LogBarnesG" "LogGamma" "LogGammaDistribution"
"LogicalExpand" "LogIntegral" "LogisticDistribution" "LogisticSigmoid"
"LogitModelFit" "LogLikelihood" "LogLinearPlot"
"LogLogisticDistribution" "LogLogPlot" "LogMultinormalDistribution"
"LogNormalDistribution" "LogPlot" "LogRankTest"
"LogSeriesDistribution" "LongEqual" "Longest" "LongestCommonSequence"
"LongestCommonSequencePositions" "LongestCommonSubsequence"
"LongestCommonSubsequencePositions" "LongestMatch"
"LongestOrderedSequence" "LongForm" "Longitude" "LongLeftArrow"
"LongLeftRightArrow" "LongRightArrow" "LongShortTermMemoryLayer"
"Lookup" "Loopback" "LoopFreeGraphQ" "Looping" "LossFunction"
"LowerCaseQ" "LowerLeftArrow" "LowerRightArrow" "LowerTriangularize"
"LowerTriangularMatrixQ" "LowpassFilter" "LQEstimatorGains"
"LQGRegulator" "LQOutputRegulatorGains" "LQRegulatorGains"
"LUBackSubstitution" "LucasL" "LuccioSamiComponents" "LUDecomposition"
"LunarEclipse" "LUVColor" "LyapunovSolve" "LyonsGroupLy" "MachineID"
"MachineName" "MachineNumberQ" "MachinePrecision"
"MacintoshSystemPageSetup" "Magenta" "Magnification" "Magnify"
"MailAddressValidation" "MailExecute" "MailFolder" "MailItem"
"MailReceiverFunction" "MailResponseFunction" "MailSearch"
"MailServerConnect" "MailServerConnection" "MailSettings" "MainSolve"
"MaintainDynamicCaches" "Majority" "MakeBoxes" "MakeExpression"
"MakeRules" "ManagedLibraryExpressionID" "ManagedLibraryExpressionQ"
"MandelbrotSetBoettcher" "MandelbrotSetDistance"
"MandelbrotSetIterationCount" "MandelbrotSetMemberQ"
"MandelbrotSetPlot" "MangoldtLambda" "ManhattanDistance" "Manipulate"
"Manipulator" "MannedSpaceMissionData" "MannWhitneyTest"
"MantissaExponent" "Manual" "Map" "MapAll" "MapAt" "MapIndexed"
"MAProcess" "MapThread" "MarchenkoPasturDistribution" "MarcumQ"
"MardiaCombinedTest" "MardiaKurtosisTest" "MardiaSkewnessTest"
"MarginalDistribution" "MarkovProcessProperties" "Masking"
"MassConcentrationCondition" "MassFluxValue"
"MassImpermeableBoundaryValue" "MassOutflowValue" "MassSymmetryValue"
"MassTransferValue" "MassTransportPDEComponent"
"MatchingDissimilarity" "MatchLocalNameQ" "MatchLocalNames" "MatchQ"
"Material" "MaternPointProcess" "MathematicalFunctionData"
"MathematicaNotation" "MathieuC" "MathieuCharacteristicA"
"MathieuCharacteristicB" "MathieuCharacteristicExponent"
"MathieuCPrime" "MathieuGroupM11" "MathieuGroupM12" "MathieuGroupM22"
"MathieuGroupM23" "MathieuGroupM24" "MathieuS" "MathieuSPrime"
"MathMLForm" "MathMLText" "Matrices" "MatrixExp" "MatrixForm"
"MatrixFunction" "MatrixLog" "MatrixNormalDistribution" "MatrixPlot"
"MatrixPower" "MatrixPropertyDistribution" "MatrixQ" "MatrixRank"
"MatrixTDistribution" "Max" "MaxBend" "MaxCellMeasure"
"MaxColorDistance" "MaxDate" "MaxDetect" "MaxDuration"
"MaxExtraBandwidths" "MaxExtraConditions" "MaxFeatureDisplacement"
"MaxFeatures" "MaxFilter" "MaximalBy" "Maximize" "MaxItems"
"MaxIterations" "MaxLimit" "MaxMemoryUsed" "MaxMixtureKernels"
"MaxOverlapFraction" "MaxPlotPoints" "MaxPoints" "MaxRecursion"
"MaxStableDistribution" "MaxStepFraction" "MaxSteps" "MaxStepSize"
"MaxTrainingRounds" "MaxValue" "MaxwellDistribution" "MaxWordGap"
"McLaughlinGroupMcL" "Mean" "MeanAbsoluteLossLayer" "MeanAround"
"MeanClusteringCoefficient" "MeanDegreeConnectivity" "MeanDeviation"
"MeanFilter" "MeanGraphDistance" "MeanNeighborDegree"
"MeanPointDensity" "MeanShift" "MeanShiftFilter"
"MeanSquaredLossLayer" "Median" "MedianDeviation" "MedianFilter"
"MedicalTestData" "Medium" "MeijerG" "MeijerGReduce"
"MeixnerDistribution" "MellinConvolve" "MellinTransform" "MemberQ"
"MemoryAvailable" "MemoryConstrained" "MemoryConstraint" "MemoryInUse"
"MengerMesh" "Menu" "MenuAppearance" "MenuCommandKey" "MenuEvaluator"
"MenuItem" "MenuList" "MenuPacket" "MenuSortingValue" "MenuStyle"
"MenuView" "Merge" "MergeDifferences" "MergeDocumentationIndex"
"MergeDocumentationIndexes" "MergingFunction" "MersennePrimeExponent"
"MersennePrimeExponentQ" "Mesh" "MeshCellCentroid" "MeshCellCount"
"MeshCellHighlight" "MeshCellIndex" "MeshCellLabel" "MeshCellMarker"
"MeshCellMeasure" "MeshCellQuality" "MeshCells"
"MeshCellShapeFunction" "MeshCellStyle" "MeshConnectivityGraph"
"MeshCoordinates" "MeshFunctions" "MeshPrimitives" "MeshQualityGoal"
"MeshRange" "MeshRefinementFunction" "MeshRegion" "MeshRegionQ"
"MeshShading" "MeshStyle" "Message" "MessageDialog" "MessageList"
"MessageName" "MessageObject" "MessageOptions" "MessagePacket"
"Messages" "MessagesNotebook" "MetaCharacters" "MetaInformation"
"MeteorShowerData" "Method" "MethodOptions" "MexicanHatWavelet"
"MeyerWavelet" "Midpoint" "Min" "MinColorDistance" "MinDate"
"MinDetect" "MineralData" "MinFilter" "MinimalBy" "MinimalPolynomial"
"MinimalStateSpaceModel" "Minimize" "MinimumTimeIncrement"
"MinIntervalSize" "MinkowskiQuestionMark" "MinLimit" "MinMax"
"MinorPlanetData" "Minors" "MinRecursion" "MinSize"
"MinStableDistribution" "Minus" "MinusPlus" "MinValue" "Missing"
"MissingBehavior" "MissingDataMethod" "MissingDataRules" "MissingQ"
"MissingString" "MissingStyle" "MissingValuePattern" "MittagLefflerE"
"MixedFractionParts" "MixedGraphQ" "MixedMagnitude" "MixedRadix"
"MixedRadixQuantity" "MixedUnit" "MixtureDistribution" "Mod" "Modal"
"Mode" "Modular" "ModularInverse" "ModularLambda" "Module" "Modulus"
"MoebiusMu" "Molecule" "MoleculeContainsQ" "MoleculeDraw"
"MoleculeEquivalentQ" "MoleculeGraph" "MoleculeModify"
"MoleculePattern" "MoleculePlot" "MoleculePlot3D" "MoleculeProperty"
"MoleculeQ" "MoleculeRecognize" "MoleculeValue" "Moment"
"MomentConvert" "MomentEvaluate" "MomentGeneratingFunction"
"MomentOfInertia" "Monday" "Monitor" "MonomialList" "MonomialOrder"
"MonsterGroupM" "MoonPhase" "MoonPosition" "MorletWavelet"
"MorphologicalBinarize" "MorphologicalBranchPoints"
"MorphologicalComponents" "MorphologicalEulerNumber"
"MorphologicalGraph" "MorphologicalPerimeter" "MorphologicalTransform"
"MortalityData" "Most" "MountainData" "MouseAnnotation"
"MouseAppearance" "MouseAppearanceTag" "MouseButtons" "Mouseover"
"MousePointerNote" "MousePosition" "MovieData" "MovingAverage"
"MovingMap" "MovingMedian" "MoyalDistribution" "Multicolumn"
"MultiedgeStyle" "MultigraphQ" "MultilaunchWarning"
"MultiLetterItalics" "MultiLetterStyle" "MultilineFunction"
"Multinomial" "MultinomialDistribution" "MultinormalDistribution"
"MultiplicativeOrder" "Multiplicity" "MultiplySides" "MultiscriptBox"
"Multiselection" "MultivariateHypergeometricDistribution"
"MultivariatePoissonDistribution" "MultivariateTDistribution"))

(defvar xah-wolfram-funs3 nil "List of Wolfram Language symbols. Part of many.")
(setq xah-wolfram-funs3 '(
"N"
"NakagamiDistribution" "NameQ" "Names" "NamespaceBox"
"NamespaceBoxOptions" "Nand" "NArgMax" "NArgMin" "NBernoulliB"
"NBodySimulation" "NBodySimulationData" "NCache" "NDEigensystem"
"NDEigenvalues" "NDSolve" "NDSolveValue" "Nearest" "NearestFunction"
"NearestMeshCells" "NearestNeighborG" "NearestNeighborGraph"
"NearestTo" "NebulaData" "NeedCurrentFrontEndPackagePacket"
"NeedCurrentFrontEndSymbolsPacket" "NeedlemanWunschSimilarity" "Needs"
"Negative" "NegativeBinomialDistribution" "NegativeDefiniteMatrixQ"
"NegativeIntegers" "NegativelyOrientedPoints"
"NegativeMultinomialDistribution" "NegativeRationals" "NegativeReals"
"NegativeSemidefiniteMatrixQ" "NeighborhoodData" "NeighborhoodGraph"
"Nest" "NestedGreaterGreater" "NestedLessLess" "NestedScriptRules"
"NestGraph" "NestList" "NestWhile" "NestWhileList" "NetAppend"
"NetArray" "NetArrayLayer" "NetBidirectionalOperator" "NetChain"
"NetDecoder" "NetDelete" "NetDrop" "NetEncoder" "NetEvaluationMode"
"NetExtract" "NetFlatten" "NetFoldOperator" "NetGANOperator"
"NetGraph" "NetInformation" "NetInitialize" "NetInsert"
"NetInsertSharedArrays" "NetJoin" "NetMapOperator"
"NetMapThreadOperator" "NetMeasurements" "NetModel" "NetNestOperator"
"NetPairEmbeddingOperator" "NetPort" "NetPortGradient" "NetPrepend"
"NetRename" "NetReplace" "NetReplacePart" "NetSharedArray"
"NetStateObject" "NetTake" "NetTrain" "NetTrainResultsObject"
"NetworkPacketCapture" "NetworkPacketRecording"
"NetworkPacketRecordingDuring" "NetworkPacketTrace" "NeumannValue"
"NevilleThetaC" "NevilleThetaD" "NevilleThetaN" "NevilleThetaS"
"NewDocumentationIndexMerger" "NewDocumentationNotebookIndexer"
"NewPrimitiveStyle" "NExpectation" "Next" "NextCell" "NextDate"
"NextPrime" "NextScheduledTaskTime" "NeymanScottPointProcess"
"NHoldAll" "NHoldFirst" "NHoldRest" "NicholsGridLines" "NicholsPlot"
"NightHemisphere" "NIntegrate" "NMaximize" "NMaxValue" "NMinimize"
"NMinValue" "NominalVariables" "NonAssociative"
"NoncentralBetaDistribution" "NoncentralChiSquareDistribution"
"NoncentralFRatioDistribution" "NoncentralStudentTDistribution"
"NonCommutativeMultiply" "NonConstants"
"NondimensionalizationTransform" "None" "NoneTrue" "NonlinearModelFit"
"NonlinearStateSpaceModel" "NonlocalMeansFilter" "NonNegative"
"NonNegativeIntegers" "NonNegativeRationals" "NonNegativeReals"
"NonPositive" "NonPositiveIntegers" "NonPositiveRationals"
"NonPositiveReals" "Nor" "NorlundB" "Norm" "Normal"
"NormalDistribution" "NormalGrouping" "NormalizationLayer" "Normalize"
"Normalized" "NormalizedSquaredEuclideanDistance" "NormalMatrixQ"
"NormalsFunction" "NormFunction" "Not" "NotCongruent" "NotCupCap"
"NotDoubleVerticalBar" "Notebook" "NotebookApply" "NotebookAutoSave"
"NotebookBrowseDirectory" "NotebookClose" "NotebookConvertSettings"
"NotebookCreate" "NotebookCreateReturnObject" "NotebookDefault"
"NotebookDelete" "NotebookDirectory" "NotebookDynamicExpression"
"NotebookEvaluate" "NotebookEventActions" "NotebookFileName"
"NotebookFind" "NotebookFindReturnObject" "NotebookGet"
"NotebookGetLayoutInformationPacket" "NotebookGetMisspellingsPacket"
"NotebookImport" "NotebookInformation" "NotebookInterfaceObject"
"NotebookLocate" "NotebookObject" "NotebookOpen"
"NotebookOpenReturnObject" "NotebookPath" "NotebookPrint"
"NotebookPut" "NotebookPutReturnObject" "NotebookRead"
"NotebookResetGeneratedCells" "Notebooks" "NotebookSave"
"NotebookSaveAs" "NotebookSelection"
"NotebookSetupLayoutInformationPacket" "NotebooksMenu"
"NotebookTemplate" "NotebookWrite" "NotElement" "NotEqualTilde"
"NotExists" "NotGreater" "NotGreaterEqual" "NotGreaterFullEqual"
"NotGreaterGreater" "NotGreaterLess" "NotGreaterSlantEqual"
"NotGreaterTilde" "Nothing" "NotHumpDownHump" "NotHumpEqual"
"NotificationFunction" "NotLeftTriangle" "NotLeftTriangleBar"
"NotLeftTriangleEqual" "NotLess" "NotLessEqual" "NotLessFullEqual"
"NotLessGreater" "NotLessLess" "NotLessSlantEqual" "NotLessTilde"
"NotNestedGreaterGreater" "NotNestedLessLess" "NotPrecedes"
"NotPrecedesEqual" "NotPrecedesSlantEqual" "NotPrecedesTilde"
"NotReverseElement" "NotRightTriangle" "NotRightTriangleBar"
"NotRightTriangleEqual" "NotSquareSubset" "NotSquareSubsetEqual"
"NotSquareSuperset" "NotSquareSupersetEqual" "NotSubset"
"NotSubsetEqual" "NotSucceeds" "NotSucceedsEqual"
"NotSucceedsSlantEqual" "NotSucceedsTilde" "NotSuperset"
"NotSupersetEqual" "NotTilde" "NotTildeEqual" "NotTildeFullEqual"
"NotTildeTilde" "NotVerticalBar" "Now" "NoWhitespace" "NProbability"
"NProduct" "NProductFactors" "NRoots" "NSolve" "NSum" "NSumTerms"
"NuclearExplosionData" "NuclearReactorData" "Null" "NullRecords"
"NullSpace" "NullWords" "Number" "NumberCompose" "NumberDecompose"
"NumberExpand" "NumberFieldClassNumber" "NumberFieldDiscriminant"
"NumberFieldFundamentalUnits" "NumberFieldIntegralBasis"
"NumberFieldNormRepresentatives" "NumberFieldRegulator"
"NumberFieldRootsOfUnity" "NumberFieldSignature" "NumberForm"
"NumberFormat" "NumberLinePlot" "NumberMarks" "NumberMultiplier"
"NumberPadding" "NumberPoint" "NumberQ" "NumberSeparator"
"NumberSigns" "NumberString" "Numerator" "NumeratorDenominator"
"NumericalOrder" "NumericalSort" "NumericArray" "NumericArrayQ"
"NumericArrayType" "NumericFunction" "NumericQ" "NuttallWindow"
"NValues" "NyquistGridLines" "NyquistPlot"
"O" "ObjectExistsQ"
"ObservabilityGramian" "ObservabilityMatrix" "ObservableDecomposition"
"ObservableModelQ" "OceanData" "Octahedron" "OddQ" "Off" "Offset"
"OLEData" "On" "ONanGroupON" "Once" "OneIdentity" "Opacity"
"OpacityFunction" "OpacityFunctionScaling" "Open" "OpenAppend"
"Opener" "OpenerBox" "OpenerBoxOptions" "OpenerView"
"OpenFunctionInspectorPacket" "Opening" "OpenRead"
"OpenSpecialOptions" "OpenTemporary" "OpenWrite" "Operate"
"OperatingSystem" "OperatorApplied" "OptimumFlowData" "Optional"
"OptionalElement" "OptionInspectorSettings" "OptionQ" "Options"
"OptionsPacket" "OptionsPattern" "OptionValue" "OptionValueBox"
"OptionValueBoxOptions" "Or" "Orange" "Order" "OrderDistribution"
"OrderedQ" "Ordering" "OrderingBy" "OrderingLayer" "Orderless"
"OrderlessPatternSequence" "OrnsteinUhlenbeckProcess" "Orthogonalize"
"OrthogonalMatrixQ" "Out" "Outer" "OuterPolygon" "OuterPolyhedron"
"OutputAutoOverwrite" "OutputControllabilityMatrix"
"OutputControllableModelQ" "OutputForm" "OutputFormData"
"OutputGrouping" "OutputMathEditExpression" "OutputNamePacket"
"OutputPorts" "OutputResponse" "OutputSizeLimit" "OutputStream" "Over"
"OverBar" "OverDot" "Overflow" "OverHat" "Overlaps" "Overlay"
"OverlayBox" "OverlayBoxOptions" "Overscript" "OverscriptBox"
"OverscriptBoxOptions" "OverTilde" "OverVector" "OverwriteTarget"
"OwenT" "OwnValues"
 ))

(defvar xah-wolfram-funs3-5 nil "List of Wolfram Language symbols. Part of many.")
(setq xah-wolfram-funs3-5 '(
"Package" "PackingMethod" "PackPaclet"
"PacletDataRebuild" "PacletDirectoryAdd" "PacletDirectoryLoad"
"PacletDirectoryRemove" "PacletDirectoryUnload" "PacletDisable"
"PacletEnable" "PacletFind" "PacletFindRemote" "PacletInformation"
"PacletInstall" "PacletInstallSubmit" "PacletNewerQ" "PacletObject"
"PacletObjectQ" "PacletSite" "PacletSiteObject" "PacletSiteRegister"
"PacletSites" "PacletSiteUnregister" "PacletSiteUpdate"
"PacletUninstall" "PacletUpdate" "PaddedForm" "Padding" "PaddingLayer"
"PaddingSize" "PadeApproximant" "PadLeft" "PadRight" "PageBreakAbove"
"PageBreakBelow" "PageBreakWithin" "PageFooterLines" "PageFooters"
"PageHeaderLines" "PageHeaders" "PageHeight" "PageRankCentrality"
"PageTheme" "PageWidth" "Pagination" "PairCorrelationG"
"PairedBarChart" "PairedHistogram" "PairedSmoothHistogram"
"PairedTTest" "PairedZTest" "PaletteNotebook" "PalettePath"
"PalindromeQ" "Pane" "PaneBox" "PaneBoxOptions" "Panel" "PanelBox"
"PanelBoxOptions" "Paneled" "PaneSelector" "PaneSelectorBox"
"PaneSelectorBoxOptions" "PaperWidth" "ParabolicCylinderD"
"ParagraphIndent" "ParagraphSpacing" "ParallelArray"
"ParallelAxisPlot" "ParallelCombine" "ParallelDo" "Parallelepiped"
"ParallelEvaluate" "Parallelization" "Parallelize" "ParallelMap"
"ParallelNeeds" "Parallelogram" "ParallelProduct" "ParallelSubmit"
"ParallelSum" "ParallelTable" "ParallelTry" "Parameter"
"ParameterEstimator" "ParameterMixtureDistribution"
"ParameterVariables" "ParametricConvexOptimization"
"ParametricFunction" "ParametricNDSolve" "ParametricNDSolveValue"
"ParametricPlot" "ParametricPlot3D" "ParametricRampLayer"
"ParametricRegion" "ParentBox" "ParentCell" "ParentConnect"
"ParentDirectory" "ParentForm" "Parenthesize" "ParentList"
"ParentNotebook" "ParetoDistribution" "ParetoPickandsDistribution"
"ParkData" "Part" "PartBehavior" "PartialCorrelationFunction"
"PartialD" "ParticleAcceleratorData" "ParticleData" "Partition"
"PartitionGranularity" "PartitionsP" "PartitionsQ" "PartLayer"
"PartOfSpeech" "PartProtection" "ParzenWindow" "PascalDistribution"
"PassEventsDown" "PassEventsUp" "Paste" "PasteAutoQuoteCharacters"
"PasteBoxFormInlineCells" "PasteButton" "Path" "PathGraph"
"PathGraphQ" "Pattern" "PatternFilling" "PatternSequence"
"PatternTest" "PauliMatrix" "PaulWavelet" "Pause" "PausedTime" "PDF"
"PeakDetect" "PeanoCurve" "PearsonChiSquareTest"
"PearsonCorrelationTest" "PearsonDistribution" "PenttinenPointProcess"
"PercentForm" "PerfectNumber" "PerfectNumberQ" "PerformanceGoal"
"Perimeter" "PeriodicBoundaryCondition" "PeriodicInterpolation"
"Periodogram" "PeriodogramArray" "Permanent" "Permissions"
"PermissionsGroup" "PermissionsGroupMemberQ" "PermissionsGroups"
"PermissionsKey" "PermissionsKeys" "PermutationCycles"
"PermutationCyclesQ" "PermutationGroup" "PermutationLength"
"PermutationList" "PermutationListQ" "PermutationMax" "PermutationMin"
"PermutationOrder" "PermutationPower" "PermutationProduct"
"PermutationReplace" "Permutations" "PermutationSupport" "Permute"
"PeronaMalikFilter" "Perpendicular" "PerpendicularBisector"
"PersistenceLocation" "PersistenceTime" "PersistentObject"
"PersistentObjects" "PersistentValue" "PersonData" "PERTDistribution"
"PetersenGraph" "PhaseMargins" "PhaseRange" "PhysicalSystemData" "Pi"
"Pick" "PickedElements" "PickMode" "PIDData" "PIDDerivativeFilter"
"PIDFeedforward" "PIDTune" "Piecewise" "PiecewiseExpand" "PieChart"
"PieChart3D" "PillaiTrace" "PillaiTraceTest" "PingTime" "Pink"
"PitchRecognize" "Pivoting" "PixelConstrained" "PixelValue"
"PixelValuePositions" "Placed" "Placeholder" "PlaceholderLayer"
"PlaceholderReplace" "Plain" "PlanarAngle" "PlanarGraph"
"PlanarGraphQ" "PlanckRadiationLaw" "PlaneCurveData"
"PlanetaryMoonData" "PlanetData" "PlantData" "Play" "PlayRange" "Plot"
"Plot3D" "Plot3Matrix" "PlotDivision" "PlotJoined" "PlotLabel"
"PlotLabels" "PlotLayout" "PlotLegends" "PlotMarkers" "PlotPoints"
"PlotRange" "PlotRangeClipping" "PlotRangeClipPlanesStyle"
"PlotRangePadding" "PlotRegion" "PlotStyle" "PlotTheme" "Pluralize"
"Plus" "PlusMinus" "Pochhammer" "PodStates" "PodWidth" "Point"
"Point3DBox" "Point3DBoxOptions" "PointBox" "PointBoxOptions"
"PointCountDistribution" "PointDensity" "PointDensityFunction"
"PointFigureChart" "PointLegend" "PointProcessEstimator"
"PointProcessFitTest" "PointProcessParameterAssumptions"
"PointProcessParameterQ" "PointSize" "PointStatisticFunction"
"PointValuePlot" "PoissonConsulDistribution" "PoissonDistribution"
"PoissonPDEComponent" "PoissonPointProcess" "PoissonProcess"
"PoissonWindow" "PolarAxes" "PolarAxesOrigin" "PolarGridLines"
"PolarPlot" "PolarTicks" "PoleZeroMarkers" "PolyaAeppliDistribution"
"PolyGamma" "Polygon" "Polygon3DBox" "Polygon3DBoxOptions"
"PolygonalNumber" "PolygonAngle" "PolygonBox" "PolygonBoxOptions"
"PolygonCoordinates" "PolygonDecomposition" "PolygonHoleScale"
"PolygonIntersections" "PolygonScale" "Polyhedron" "PolyhedronAngle"
"PolyhedronCoordinates" "PolyhedronData" "PolyhedronDecomposition"
"PolyhedronGenus" "PolyLog" "PolynomialExpressionQ"
"PolynomialExtendedGCD" "PolynomialForm" "PolynomialGCD"
"PolynomialLCM" "PolynomialMod" "PolynomialQ" "PolynomialQuotient"
"PolynomialQuotientRemainder" "PolynomialReduce" "PolynomialRemainder"
"Polynomials" "PoolingLayer" "PopupMenu" "PopupMenuBox"
"PopupMenuBoxOptions" "PopupView" "PopupWindow" "Position"
"PositionIndex" "Positive" "PositiveDefiniteMatrixQ"
"PositiveIntegers" "PositivelyOrientedPoints" "PositiveRationals"
"PositiveReals" "PositiveSemidefiniteMatrixQ" "PossibleZeroQ"
"Postfix" "PostScript" "Power" "PowerDistribution" "PowerExpand"
"PowerMod" "PowerModList" "PowerRange" "PowerSpectralDensity"
"PowersRepresentations" "PowerSymmetricPolynomial" "Precedence"
"PrecedenceForm" "Precedes" "PrecedesEqual" "PrecedesSlantEqual"
"PrecedesTilde" "Precision" "PrecisionGoal" "PreDecrement" "Predict"
"PredictionRoot" "PredictorFunction" "PredictorInformation"
"PredictorMeasurements" "PredictorMeasurementsObject" "PreemptProtect"
"PreferencesDelete" "PreferencesPath" "PreferencesRead"
"PreferencesWrite" "Prefix" "PreIncrement" "Prepend" "PrependLayer"
"PrependTo" "PreprocessingRules" "PreserveColor"
"PreserveImageOptions" "Previous" "PreviousCell" "PreviousDate"
"PriceGraphDistribution" "PrimaryPlaceholder" "Prime" "PrimeNu"
"PrimeOmega" "PrimePi" "PrimePowerQ" "PrimeQ" "Primes" "PrimeZetaP"
"PrimitivePolynomialQ" "PrimitiveRoot" "PrimitiveRootList"
"PrincipalComponents" "PrincipalValue" "Print" "PrintableASCIIQ"
"PrintAction" "PrintForm" "PrintingCopies" "PrintingOptions"
"PrintingPageRange" "PrintingStartingPageNumber"
"PrintingStyleEnvironment" "Printout3D" "Printout3DPreviewer"
"PrintPrecision" "PrintTemporary" "Prism" "PrismBox" "PrismBoxOptions"
"PrivateCellOptions" "PrivateEvaluationOptions" "PrivateFontOptions"
"PrivateFrontEndOptions" "PrivateKey" "PrivateNotebookOptions"
"PrivatePaths" "Probability" "ProbabilityDistribution"
"ProbabilityPlot" "ProbabilityPr" "ProbabilityScalePlot"
"ProbitModelFit" "ProcessConnection" "ProcessDirectory"
"ProcessEnvironment" "Processes" "ProcessEstimator"
"ProcessInformation" "ProcessObject" "ProcessParameterAssumptions"
"ProcessParameterQ" "ProcessStateDomain" "ProcessStatus"
"ProcessTimeDomain" "Product" "ProductDistribution" "ProductLog"
"ProgressIndicator" "ProgressIndicatorBox"
"ProgressIndicatorBoxOptions" "ProgressReporting" "Projection"
"Prolog" "PromptForm" "ProofObject" "Properties" "Property"
"PropertyList" "PropertyValue" "Proportion" "Proportional" "Protect"
"Protected" "ProteinData" "Pruning" "PseudoInverse"
"PsychrometricPropertyData" "PublicKey" "PublisherID" "PulsarData"
"PunctuationCharacter" "Purple" "Put" "PutAppend" "Pyramid"
"PyramidBox" "PyramidBoxOptions" "QBinomial" "QFactorial" "QGamma"
"QHypergeometricPFQ" "QnDispersion" "QPochhammer" "QPolyGamma"
"QRDecomposition" "QuadraticIrrationalQ" "QuadraticOptimization"
"Quantile" "QuantilePlot" "Quantity" "QuantityArray"
"QuantityDistribution" "QuantityForm" "QuantityMagnitude" "QuantityQ"
"QuantityUnit" "QuantityVariable" "QuantityVariableCanonicalUnit"
"QuantityVariableDimensions" "QuantityVariableIdentifier"
"QuantityVariablePhysicalQuantity" "Quartics" "QuartileDeviation"
"Quartiles" "QuartileSkewness" "Query" "QueueingNetworkProcess"
"QueueingProcess" "QueueProperties" "Quiet" "QuietEcho" "Quit"
"Quotient" "QuotientRemainder" "RadialAxisPlot"
"RadialGradientFilling" "RadialGradientImage" "RadialityCentrality"
"RadicalBox" "RadicalBoxOptions" "RadioButton" "RadioButtonBar"
"RadioButtonBox" "RadioButtonBoxOptions" "Radon" "RadonTransform"
"RamanujanTau" "RamanujanTauL" "RamanujanTauTheta" "RamanujanTauZ"
"Ramp" "Random" "RandomArrayLayer" "RandomChoice" "RandomColor"
"RandomComplex" "RandomEntity" "RandomFunction" "RandomGeneratorState"
"RandomGeoPosition" "RandomGraph" "RandomImage" "RandomInstance"
"RandomInteger" "RandomPermutation" "RandomPoint"
"RandomPointConfiguration" "RandomPolygon" "RandomPolyhedron"
"RandomPrime" "RandomReal" "RandomSample" "RandomSeed" "RandomSeeding"
"RandomVariate" "RandomWalkProcess" "RandomWord" "Range" "RangeFilter"
"RangeSpecification" "RankedMax" "RankedMin" "RarerProbability"
"Raster" "Raster3D" "Raster3DBox" "Raster3DBoxOptions"
"Raster3DBoxOptionsImageEditMode" "RasterArray" "RasterBox"
"RasterBoxOptions" "RasterBoxOptionsImageEditMode" "Rasterize"
"RasterSize" "Rational" "RationalExpressionQ" "RationalFunctions"
"Rationalize" "Rationals" "Ratios" "RawArray" "RawBoxes" "RawData"
"RawMedium" "RayleighDistribution" "Re" "ReactionPDETerm" "Read"
"ReadByteArray" "ReadLine" "ReadList" "ReadProtected" "ReadString"
"Real" "RealAbs" "RealBlockDiagonalForm" "RealDigits" "RealExponent"
"Reals" "RealSign" "Reap" "RebuildPacletData" "RecognitionPrior"
"RecognitionThreshold" "Record" "RecordLists" "RecordSeparators"
"Rectangle" "RectangleBox" "RectangleBoxOptions" "RectangleChart"
"RectangleChart3D" "RectangularRepeatingElement" "RecurrenceFilter"
"RecurrenceTable" "RecurringDigitsForm" "Red" "Reduce" "RefBox"
"ReferenceLineStyle" "ReferenceMarkers" "ReferenceMarkerStyle"
"Refine" "ReflectionMatrix" "ReflectionTransform" "Refresh"
"RefreshRate" "Region" "RegionBinarize" "RegionBoundary"
"RegionBoundaryStyle" "RegionBounds" "RegionCentroid"
"RegionDifference" "RegionDimension" "RegionDisjoint" "RegionDistance"
"RegionDistanceFunction" "RegionEmbeddingDimension" "RegionEqual"
"RegionFillingStyle" "RegionFunction" "RegionImage"
"RegionIntersection" "RegionMeasure" "RegionMember"
"RegionMemberFunction" "RegionMoment" "RegionNearest"
"RegionNearestFunction" "RegionPlot" "RegionPlot3D" "RegionProduct"
"RegionQ" "RegionResize" "RegionSize" "RegionSymmetricDifference"
"RegionUnion" "RegionWithin" "RegisterExternalEvaluator"
"RegisterListener" "RegularExpression" "Regularization"
"RegularlySampledQ" "RegularPolygon" "ReIm" "ReImLabels" "ReImPlot"
"ReImStyle" "ReindexLegacyPacletsAndSearch" "Reinstall"
"RelationalDatabase" "RelationGraph" "Release" "ReleaseHold"
"ReliabilityDistribution" "ReliefImage" "ReliefPlot"
"RemoteAuthorizationCaching" "RemoteBatchJobAbort"
"RemoteBatchJobObject" "RemoteBatchJobs" "RemoteBatchMapSubmit"
"RemoteBatchSubmissionEnvironment" "RemoteBatchSubmit" "RemoteConnect"
"RemoteConnectionObject" "RemoteEvaluate" "RemoteFile"
"RemoteInputFiles" "RemoteKernelObject" "RemoteProviderSettings"
"RemoteRun" "RemoteRunProcess" "RemovalConditions" "Remove"
"RemoveAlphaChannel" "RemoveAsynchronousTask" "RemoveAudioStream"
"RemoveBackground" "RemoveChannelListener" "RemoveChannelSubscribers"
"Removed" "RemoveDiacritics" "RemoveInputStreamMethod"
"RemoveOutputStreamMethod" "RemoveProperty" "RemoveScheduledTask"
"RemoveUsers" "RemoveVideoStream" "RenameDirectory" "RenameFile"
"RenderAll" "RenderingOptions" "RenewalProcess" "RenkoChart"
"RepairMesh" "Repeated" "RepeatedNull" "RepeatedString"
"RepeatedTiming" "RepeatingElement" "Replace" "ReplaceAll"
"ReplaceHeldPart" "ReplaceImageValue" "ReplaceList" "ReplacePart"
"ReplacePixelValue" "ReplaceRepeated" "ReplicateLayer"
"RequiredPhysicalQuantities" "Resampling" "ResamplingAlgorithmData"
"ResamplingMethod" "Rescale" "RescalingTransform" "ResetDirectory"
"ResetMenusPacket" "ResetScheduledTask" "ReshapeLayer" "Residue"
"ResizeLayer" "Resolution" "Resolve" "ResourceAcquire" "ResourceAdd"
"ResourceData" "ResourceFunction" "ResourceObject" "ResourceRegister"
"ResourceRemove" "ResourceSearch" "ResourcesLocate"
"ResourceSubmissionObject" "ResourceSubmit" "ResourceSystemBase"
"ResourceSystemPath" "ResourceUpdate" "ResourceVersion" "ResponseForm"
"Rest" "RestartInterval" "Restricted" "Resultant" "ResumePacket"
"Return" "ReturnEntersInput" "ReturnExpressionPacket"
"ReturnInputFormPacket" "ReturnPacket" "ReturnReceiptFunction"
"ReturnTextPacket" "Reverse" "ReverseApplied"
"ReverseBiorthogonalSplineWavelet" "ReverseElement"
"ReverseEquilibrium" "ReverseGraph" "ReverseSort" "ReverseSortBy"
"ReverseUpEquilibrium" "RevolutionAxis" "RevolutionPlot3D" "RGBColor"
"RiccatiSolve" "RiceDistribution" "RidgeFilter" "RiemannR"
"RiemannSiegelTheta" "RiemannSiegelZ" "RiemannXi" "Riffle" "Right"
"RightArrow" "RightArrowBar" "RightArrowLeftArrow" "RightComposition"
"RightCosetRepresentative" "RightDownTeeVector" "RightDownVector"
"RightDownVectorBar" "RightTee" "RightTeeArrow" "RightTeeVector"
"RightTriangle" "RightTriangleBar" "RightTriangleEqual"
"RightUpDownVector" "RightUpTeeVector" "RightUpVector"
"RightUpVectorBar" "RightVector" "RightVectorBar" "RipleyK"
"RipleyRassonRegion" "RiskAchievementImportance"
"RiskReductionImportance" "RobustConvexOptimization"
"RogersTanimotoDissimilarity" "RollPitchYawAngles"
"RollPitchYawMatrix" "RomanNumeral" "Root" "RootApproximant"
"RootIntervals" "RootLocusPlot" "RootMeanSquare" "RootOfUnityQ"
"RootReduce" "Roots" "RootSum" "Rotate" "RotateLabel" "RotateLeft"
"RotateRight" "RotationAction" "RotationBox" "RotationBoxOptions"
"RotationMatrix" "RotationTransform" "Round" "RoundImplies"
"RoundingRadius" "Row" "RowAlignments" "RowBackgrounds" "RowBox"
"RowHeights" "RowLabels" "RowLines" "RowMinHeight" "RowReduce"
"RowsEqual" "RowSpacings" "RSolve" "RSolveValue" "RudinShapiro"
"RudvalisGroupRu" "Rule" "RuleCondition" "RuleDelayed" "RuleForm"
"RulePlot" "RulerUnits" "Run" "RunProcess" "RunScheduledTask"
"RunThrough" "RuntimeAttributes" "RuntimeOptions"
"RussellRaoDissimilarity"))

(defvar xah-wolfram-funs4 nil "List of Wolfram Language symbols. Part of many.")
(setq xah-wolfram-funs4 '(
"SameQ" "SameTest" "SameTestProperties"
"SampledEntityClass" "SampleDepth" "SampledSoundFunction"
"SampledSoundList" "SampleRate" "SamplingPeriod" "SARIMAProcess"
"SARMAProcess" "SASTriangle" "SatelliteData" "SatisfiabilityCount"
"SatisfiabilityInstances" "SatisfiableQ" "Saturday" "Save" "Saveable"
"SaveAutoDelete" "SaveConnection" "SaveDefinitions"
"SavitzkyGolayMatrix" "SawtoothWave" "Scale" "Scaled" "ScaleDivisions"
"ScaledMousePosition" "ScaleOrigin" "ScalePadding" "ScaleRanges"
"ScaleRangeStyle" "ScalingFunctions" "ScalingMatrix"
"ScalingTransform" "Scan" "ScheduledTask" "ScheduledTaskActiveQ"
"ScheduledTaskInformation" "ScheduledTaskInformationData"
"ScheduledTaskObject" "ScheduledTasks" "SchurDecomposition"
"ScientificForm" "ScientificNotationThreshold" "ScorerGi"
"ScorerGiPrime" "ScorerHi" "ScorerHiPrime" "ScreenArea"
"ScreenRectangle" "ScreenStyleEnvironment" "ScriptBaselineShifts"
"ScriptForm" "ScriptLevel" "ScriptMinSize" "ScriptRules"
"ScriptSizeMultipliers" "Scrollbars" "ScrollingOptions"
"ScrollPosition" "SearchAdjustment" "SearchDocumentation"
"SearchDocumentationMetaData" "SearchIndexObject" "SearchIndices"
"SearchQueryString" "SearchResultObject" "Sec" "Sech"
"SechDistribution" "SecondOrderConeOptimization" "SectionGrouping"
"SectorChart" "SectorChart3D" "SectorOrigin" "SectorSpacing"
"SecuredAuthenticationKey" "SecuredAuthenticationKeys"
"SecurityCertificate" "SeedRandom" "Select" "Selectable"
"SelectComponents" "SelectedCells" "SelectedNotebook" "SelectFirst"
"Selection" "SelectionAnimate" "SelectionCell"
"SelectionCellCreateCell" "SelectionCellDefaultStyle"
"SelectionCellParentStyle" "SelectionCreateCell"
"SelectionDebuggerTag" "SelectionDuplicateCell" "SelectionEvaluate"
"SelectionEvaluateCreateCell" "SelectionMove" "SelectionPlaceholder"
"SelectionSetStyle" "SelectWithContents" "SelfLoops" "SelfLoopStyle"
"SemanticImport" "SemanticImportString" "SemanticInterpretation"
"SemialgebraicComponentInstances" "SemidefiniteOptimization"
"SendMail" "SendMessage" "Sequence" "SequenceAlignment"
"SequenceAttentionLayer" "SequenceCases" "SequenceCount"
"SequenceFold" "SequenceFoldList" "SequenceForm" "SequenceHold"
"SequenceLastLayer" "SequenceMostLayer" "SequencePosition"
"SequencePredict" "SequencePredictorFunction" "SequenceReplace"
"SequenceRestLayer" "SequenceReverseLayer" "SequenceSplit" "Series"
"SeriesCoefficient" "SeriesData" "SeriesTermGoal" "ServiceConnect"
"ServiceDisconnect" "ServiceExecute" "ServiceObject" "ServiceRequest"
"ServiceResponse" "ServiceSubmit" "SessionSubmit" "SessionTime" "Set"
"SetAccuracy" "SetAlphaChannel" "SetAttributes" "Setbacks"
"SetBoxFormNamesPacket" "SetCloudDirectory" "SetCookies" "SetDelayed"
"SetDirectory" "SetEnvironment" "SetEvaluationNotebook" "SetFileDate"
"SetFileLoadingContext" "SetNotebookStatusLine" "SetOptions"
"SetOptionsPacket" "SetPermissions" "SetPrecision" "SetProperty"
"SetSecuredAuthenticationKey" "SetSelectedNotebook"
"SetSharedFunction" "SetSharedVariable" "SetSpeechParametersPacket"
"SetStreamPosition" "SetSystemModel" "SetSystemOptions" "Setter"
"SetterBar" "SetterBox" "SetterBoxOptions" "Setting" "SetUsers"
"SetValue" "Shading" "Shallow" "ShannonWavelet" "ShapiroWilkTest"
"Share" "SharingList" "Sharpen" "ShearingMatrix" "ShearingTransform"
"ShellRegion" "ShenCastanMatrix" "ShiftedGompertzDistribution"
"ShiftRegisterSequence" "Short" "ShortDownArrow" "Shortest"
"ShortestMatch" "ShortestPathFunction" "ShortLeftArrow"
"ShortRightArrow" "ShortTimeFourier" "ShortTimeFourierData"
"ShortUpArrow" "Show" "ShowAutoConvert" "ShowAutoSpellCheck"
"ShowAutoStyles" "ShowCellBracket" "ShowCellLabel" "ShowCellTags"
"ShowClosedCellArea" "ShowCodeAssist" "ShowContents" "ShowControls"
"ShowCursorTracker" "ShowGroupOpenCloseIcon" "ShowGroupOpener"
"ShowInvisibleCharacters" "ShowPageBreaks" "ShowPredictiveInterface"
"ShowSelection" "ShowShortBoxForm" "ShowSpecialCharacters"
"ShowStringCharacters" "ShowSyntaxStyles" "ShrinkingDelay"
"ShrinkWrapBoundingBox" "SiderealTime" "SiegelTheta" "SiegelTukeyTest"
"SierpinskiCurve" "SierpinskiMesh" "Sign" "Signature" "SignedRankTest"
"SignedRegionDistance" "SignificanceLevel" "SignPadding" "SignTest"
"SimilarityRules" "SimpleGraph" "SimpleGraphQ" "SimplePolygonQ"
"SimplePolyhedronQ" "Simplex" "Simplify" "Sin" "Sinc"
"SinghMaddalaDistribution" "SingleEvaluation" "SingleLetterItalics"
"SingleLetterStyle" "SingularValueDecomposition" "SingularValueList"
"SingularValuePlot" "SingularValues" "Sinh" "SinhIntegral"
"SinIntegral" "SixJSymbol" "Skeleton" "SkeletonTransform"
"SkellamDistribution" "Skewness" "SkewNormalDistribution" "SkinStyle"
"Skip" "SliceContourPlot3D" "SliceDensityPlot3D" "SliceDistribution"
"SliceVectorPlot3D" "Slider" "Slider2D" "Slider2DBox"
"Slider2DBoxOptions" "SliderBox" "SliderBoxOptions" "SlideView" "Slot"
"SlotSequence" "Small" "SmallCircle" "Smaller" "SmithDecomposition"
"SmithDelayCompensator" "SmithWatermanSimilarity"
"SmoothDensityHistogram" "SmoothHistogram" "SmoothHistogram3D"
"SmoothKernelDistribution" "SmoothPointDensity" "SnDispersion"
"Snippet" "SnubPolyhedron" "SocialMediaData" "Socket" "SocketConnect"
"SocketListen" "SocketListener" "SocketObject" "SocketOpen"
"SocketReadMessage" "SocketReadyQ" "Sockets" "SocketWaitAll"
"SocketWaitNext" "SoftmaxLayer" "SokalSneathDissimilarity"
"SolarEclipse" "SolarSystemFeatureData" "SolidAngle" "SolidData"
"SolidRegionQ" "Solve" "SolveAlways" "SolveDelayed" "Sort" "SortBy"
"SortedBy" "SortedEntityClass" "Sound" "SoundAndGraphics" "SoundNote"
"SoundVolume" "SourceLink" "SourcePDETerm" "Sow" "Space"
"SpaceCurveData" "SpaceForm" "Spacer" "Spacings" "Span"
"SpanAdjustments" "SpanCharacterRounding" "SpanFromAbove"
"SpanFromBoth" "SpanFromLeft" "SpanLineThickness" "SpanMaxSize"
"SpanMinSize" "SpanningCharacters" "SpanSymmetric" "SparseArray"
"SpatialBinnedPointData" "SpatialBoundaryCorrection"
"SpatialGraphDistribution" "SpatialJ" "SpatialMedian"
"SpatialObservationRegionQ" "SpatialPointData" "SpatialPointSelect"
"SpatialRandomnessTest" "SpatialTransformationLayer" "Speak"
"SpeakerMatchQ" "SpeakTextPacket" "SpearmanRankTest" "SpearmanRho"
"SpeciesData" "SpecificityGoal" "SpectralLineData" "Spectrogram"
"SpectrogramArray" "Specularity" "SpeechCases" "SpeechInterpreter"
"SpeechRecognize" "SpeechSynthesize" "SpellingCorrection"
"SpellingCorrectionList" "SpellingDictionaries"
"SpellingDictionariesPath" "SpellingOptions"
"SpellingSuggestionsPacket" "Sphere" "SphereBox" "SpherePoints"
"SphericalBesselJ" "SphericalBesselY" "SphericalHankelH1"
"SphericalHankelH2" "SphericalHarmonicY" "SphericalPlot3D"
"SphericalRegion" "SphericalShell" "SpheroidalEigenvalue"
"SpheroidalJoiningFactor" "SpheroidalPS" "SpheroidalPSPrime"
"SpheroidalQS" "SpheroidalQSPrime" "SpheroidalRadialFactor"
"SpheroidalS1" "SpheroidalS1Prime" "SpheroidalS2" "SpheroidalS2Prime"
"Splice" "SplicedDistribution" "SplineClosed" "SplineDegree"
"SplineKnots" "SplineWeights" "Split" "SplitBy" "SpokenString" "Sqrt"
"SqrtBox" "SqrtBoxOptions" "Square" "SquaredEuclideanDistance"
"SquareFreeQ" "SquareIntersection" "SquareMatrixQ"
"SquareRepeatingElement" "SquaresR" "SquareSubset" "SquareSubsetEqual"
"SquareSuperset" "SquareSupersetEqual" "SquareUnion" "SquareWave"
"SSSTriangle" "StabilityMargins" "StabilityMarginsStyle"
"StableDistribution" "Stack" "StackBegin" "StackComplete"
"StackedDateListPlot" "StackedListPlot" "StackInhibit" "StadiumShape"
"StandardAtmosphereData" "StandardDeviation" "StandardDeviationFilter"
"StandardForm" "Standardize" "Standardized" "StandardOceanData"
"StandbyDistribution" "Star" "StarClusterData" "StarData" "StarGraph"
"StartAsynchronousTask" "StartExternalSession" "StartingStepSize"
"StartOfLine" "StartOfString" "StartProcess" "StartScheduledTask"
"StartupSound" "StartWebSession" "StateDimensions"
"StateFeedbackGains" "StateOutputEstimator" "StateResponse"
"StateSpaceModel" "StateSpaceRealization" "StateSpaceTransform"
"StateTransformationLinearize" "StationaryDistribution"
"StationaryWaveletPacketTransform" "StationaryWaveletTransform"
"StatusArea" "StatusCentrality" "StepMonitor"
"StereochemistryElements" "StieltjesGamma" "StippleShading"
"StirlingS1" "StirlingS2" "StopAsynchronousTask" "StoppingPowerData"
"StopScheduledTask" "StrataVariables" "StratonovichProcess"
"StraussHardcorePointProcess" "StraussPointProcess"
"StreamColorFunction" "StreamColorFunctionScaling" "StreamDensityPlot"
"StreamMarkers" "StreamPlot" "StreamPoints" "StreamPosition" "Streams"
"StreamScale" "StreamStyle" "StrictInequalities" "String"
"StringBreak" "StringByteCount" "StringCases" "StringContainsQ"
"StringCount" "StringDelete" "StringDrop" "StringEndsQ"
"StringExpression" "StringExtract" "StringForm" "StringFormat"
"StringFormatQ" "StringFreeQ" "StringInsert" "StringJoin"
"StringLength" "StringMatchQ" "StringPadLeft" "StringPadRight"
"StringPart" "StringPartition" "StringPosition" "StringQ"
"StringRepeat" "StringReplace" "StringReplaceList" "StringReplacePart"
"StringReverse" "StringRiffle" "StringRotateLeft" "StringRotateRight"
"StringSkeleton" "StringSplit" "StringStartsQ" "StringTake"
"StringTemplate" "StringToByteArray" "StringToStream" "StringTrim"
"StripBoxes" "StripOnInput" "StripWrapperBoxes" "StrokeForm"
"StructuralImportance" "StructuredArray" "StructuredArrayHeadQ"
"StructuredSelection" "StruveH" "StruveL" "Stub"
"StudentTDistribution" "Style" "StyleBox" "StyleBoxAutoDelete"
"StyleData" "StyleDefinitions" "StyleForm" "StyleHints"
"StyleKeyMapping" "StyleMenuListing" "StyleNameDialogSettings"
"StyleNames" "StylePrint" "StyleSheetPath" "Subdivide" "Subfactorial"
"Subgraph" "SubMinus" "SubPlus" "SubresultantPolynomialRemainders"
"SubresultantPolynomials" "Subresultants" "Subscript" "SubscriptBox"
"SubscriptBoxOptions" "Subscripted" "Subsequences" "Subset"
"SubsetCases" "SubsetCount" "SubsetEqual" "SubsetMap" "SubsetPosition"
"SubsetQ" "SubsetReplace" "Subsets" "SubStar" "SubstitutionSystem"
"Subsuperscript" "SubsuperscriptBox" "SubsuperscriptBoxOptions"
"SubtitleEncoding" "SubtitleTrackSelection" "Subtract" "SubtractFrom"
"SubtractSides" "SubValues" "Succeeds" "SucceedsEqual"
"SucceedsSlantEqual" "SucceedsTilde" "Success" "SuchThat" "Sum"
"SumConvergence" "SummationLayer" "Sunday" "SunPosition" "Sunrise"
"Sunset" "SuperDagger" "SuperMinus" "SupernovaData" "SuperPlus"
"Superscript" "SuperscriptBox" "SuperscriptBoxOptions" "Superset"
"SupersetEqual" "SuperStar" "Surd" "SurdForm" "SurfaceAppearance"
"SurfaceArea" "SurfaceColor" "SurfaceData" "SurfaceGraphics"
"SurvivalDistribution" "SurvivalFunction" "SurvivalModel"
"SurvivalModelFit" "SuspendPacket" "SuzukiDistribution"
"SuzukiGroupSuz" "SwatchLegend" "Switch" "Symbol" "SymbolInformation"
"SymbolName" "SymletWavelet" "Symmetric" "SymmetricGroup"
"SymmetricKey" "SymmetricMatrixQ" "SymmetricPolynomial"
"SymmetricReduction" "Symmetrize" "SymmetrizedArray"
"SymmetrizedArrayRules" "SymmetrizedDependentComponents"
"SymmetrizedIndependentComponents" "SymmetrizedReplacePart"
"SynchronousInitialization" "SynchronousUpdating" "Synonyms" "Syntax"
"SyntaxForm" "SyntaxInformation" "SyntaxLength" "SyntaxPacket"
"SyntaxQ" "SynthesizeMissingValues" "SystemCredential"
"SystemCredentialData" "SystemCredentialKey" "SystemCredentialKeys"
"SystemCredentialStoreObject" "SystemDialogInput" "SystemException"
"SystemGet" "SystemHelpPath" "SystemInformation"
"SystemInformationData" "SystemInstall" "SystemModel" "SystemModeler"
"SystemModelExamples" "SystemModelLinearize"
"SystemModelParametricSimulate" "SystemModelPlot"
"SystemModelProgressReporting" "SystemModelReliability" "SystemModels"
"SystemModelSimulate" "SystemModelSimulateSensitivity"
"SystemModelSimulationData" "SystemOpen" "SystemOptions"
"SystemProcessData" "SystemProcesses" "SystemsConnectionsModel"
"SystemsModelDelay" "SystemsModelDelayApproximate"
"SystemsModelDelete" "SystemsModelDimensions" "SystemsModelExtract"
"SystemsModelFeedbackConnect" "SystemsModelLabels"
"SystemsModelLinearity" "SystemsModelMerge" "SystemsModelOrder"
"SystemsModelParallelConnect" "SystemsModelSeriesConnect"
"SystemsModelStateFeedbackConnect" "SystemsModelVectorRelativeOrders"
"SystemStub" "SystemTest" "TabFilling" "Table" "TableAlignments"
"TableDepth" "TableDirections" "TableForm" "TableHeadings"
"TableSpacing" "TableView" "TableViewBox" "TableViewBoxAlignment"
"TableViewBoxBackground" "TableViewBoxItemSize" "TableViewBoxOptions"
"TabSpacings" "TabView" "TabViewBox" "TabViewBoxOptions" "TagBox"
"TagBoxNote" "TagBoxOptions" "TaggingRules" "TagSet" "TagSetDelayed"
"TagStyle" "TagUnset" "Take" "TakeDrop" "TakeLargest" "TakeLargestBy"
"TakeList" "TakeSmallest" "TakeSmallestBy" "TakeWhile" "Tally" "Tan"
"Tanh" "TargetDevice" "TargetFunctions" "TargetSystem" "TargetUnits"
"TaskAbort" "TaskExecute" "TaskObject" "TaskRemove" "TaskResume"
"Tasks" "TaskSuspend" "TaskWait" "TautologyQ" "TelegraphProcess"
"TemplateApply" "TemplateArgBox" "TemplateBox" "TemplateBoxOptions"
"TemplateEvaluate" "TemplateExpression" "TemplateIf" "TemplateObject"
"TemplateSequence" "TemplateSlot" "TemplateSlotSequence"
"TemplateUnevaluated" "TemplateVerbatim" "TemplateWith" "TemporalData"
"TemporalRegularity" "Temporary" "TemporaryVariable" "TensorContract"
"TensorDimensions" "TensorExpand" "TensorProduct" "TensorQ"
"TensorRank" "TensorReduce" "TensorSymmetry" "TensorTranspose"
"TensorWedge" "TestID" "TestReport" "TestReportObject"
"TestResultObject" "Tetrahedron" "TetrahedronBox"
"TetrahedronBoxOptions" "TeXForm" "TeXSave" "Text" "Text3DBox"
"Text3DBoxOptions" "TextAlignment" "TextBand" "TextBoundingBox"
"TextBox" "TextCases" "TextCell" "TextClipboardType" "TextContents"
"TextData" "TextElement" "TextForm" "TextGrid" "TextJustification"
"TextLine" "TextPacket" "TextParagraph" "TextPosition" "TextRecognize"
"TextResourceFunction" "TextResourceLoad" "TextSearch"
"TextSearchReport" "TextSentences" "TextString" "TextStructure"
"TextStyle" "TextTranslation" "Texture" "TextureCoordinateFunction"
"TextureCoordinateScaling" "TextWords" "Therefore" "ThermodynamicData"
"ThermometerGauge" "Thick" "Thickness" "Thin" "Thinning" "ThisLink"
"ThomasPointProcess" "ThompsonGroupTh" "Thread" "ThreadingLayer"
"ThreeJSymbol" "Threshold" "Through" "Throw" "ThueMorse" "Thumbnail"
"Thursday" "Ticks" "TicksStyle" "TideData" "Tilde" "TildeEqual"
"TildeFullEqual" "TildeTilde" "TimeConstrained" "TimeConstraint"
"TimeDirection" "TimeFormat" "TimeGoal" "TimelinePlot" "TimeObject"
"TimeObjectQ" "TimeRemaining" "Times" "TimesBy" "TimeSeries"
"TimeSeriesAggregate" "TimeSeriesForecast" "TimeSeriesInsert"
"TimeSeriesInvertibility" "TimeSeriesMap" "TimeSeriesMapThread"
"TimeSeriesModel" "TimeSeriesModelFit" "TimeSeriesResample"
"TimeSeriesRescale" "TimeSeriesShift" "TimeSeriesThread"
"TimeSeriesWindow" "TimeUsed" "TimeValue" "TimeWarpingCorrespondence"
"TimeWarpingDistance" "TimeZone" "TimeZoneConvert" "TimeZoneOffset"
"Timing" "Tiny" "TitleGrouping" "TitsGroupT" "ToBoxes"
"ToCharacterCode" "ToColor" "ToContinuousTimeModel" "ToDate" "Today"
"ToDiscreteTimeModel" "ToEntity" "ToeplitzMatrix" "ToExpression"
"ToFileName" "Together" "Toggle" "ToggleFalse" "Toggler" "TogglerBar"
"TogglerBox" "TogglerBoxOptions" "ToHeldExpression"
"ToInvertibleTimeSeries" "TokenWords" "Tolerance" "ToLowerCase"
"Tomorrow" "ToNumberField" "TooBig" "Tooltip" "TooltipBox"
"TooltipBoxOptions" "TooltipDelay" "TooltipStyle" "ToonShading" "Top"
"TopHatTransform" "ToPolarCoordinates" "TopologicalSort" "ToRadicals"
"ToRules" "ToSphericalCoordinates" "ToString" "Total" "TotalHeight"
"TotalLayer" "TotalVariationFilter" "TotalWidth" "TouchPosition"
"TouchscreenAutoZoom" "TouchscreenControlPlacement" "ToUpperCase" "Tr"
"Trace" "TraceAbove" "TraceAction" "TraceBackward" "TraceDepth"
"TraceDialog" "TraceForward" "TraceInternal" "TraceLevel" "TraceOff"
"TraceOn" "TraceOriginal" "TracePrint" "TraceScan" "TrackedSymbols"
"TrackingFunction" "TracyWidomDistribution" "TradingChart"
"TraditionalForm" "TraditionalFunctionNotation" "TraditionalNotation"
"TraditionalOrder" "TrainingProgressCheckpointing"
"TrainingProgressFunction" "TrainingProgressMeasurements"
"TrainingProgressReporting" "TrainingStoppingCriterion"
"TrainingUpdateSchedule" "TransferFunctionCancel"
"TransferFunctionExpand" "TransferFunctionFactor"
"TransferFunctionModel" "TransferFunctionPoles"
"TransferFunctionTransform" "TransferFunctionZeros"
"TransformationClass" "TransformationFunction"
"TransformationFunctions" "TransformationMatrix"
"TransformedDistribution" "TransformedField" "TransformedProcess"
"TransformedRegion" "TransitionDirection" "TransitionDuration"
"TransitionEffect" "TransitiveClosureGraph" "TransitiveReductionGraph"
"Translate" "TranslationOptions" "TranslationTransform"
"Transliterate" "Transparent" "TransparentColor" "Transpose"
"TransposeLayer" "TrapSelection" "TravelDirections"
"TravelDirectionsData" "TravelDistance" "TravelDistanceList"
"TravelMethod" "TravelTime" "TreeForm" "TreeGraph" "TreeGraphQ"
"TreePlot" "TrendStyle" "Triangle" "TriangleCenter"
"TriangleConstruct" "TriangleConvexHull" "TriangleCreate"
"TriangleDelaunay" "TriangleDelete" "TriangleExpression"
"TriangleExpressionQ" "TriangleExpressions"
"TriangleGetElementAttributes" "TriangleGetElements"
"TriangleGetHoles" "TriangleGetNeighbors" "TriangleGetPointAttributes"
"TriangleGetPointMarkers" "TriangleGetPoints" "TriangleGetRegions"
"TriangleGetSegmentMarkers" "TriangleGetSegments"
"TriangleGetVertices" "TriangleMeasurement"
"TriangleSetElementAttributes" "TriangleSetElements"
"TriangleSetHoles" "TriangleSetPointAttributes"
"TriangleSetPointMarkers" "TriangleSetPoints" "TriangleSetRegions"
"TriangleSetSegmentMarkers" "TriangleSetSegments"
"TriangleSetTriangleAreas" "TriangleSetVertices" "TriangleTriangulate"
"TriangleWave" "TriangularDistribution" "TriangulateMesh" "Trig"
"TrigExpand" "TrigFactor" "TrigFactorList" "Trigger" "TrigReduce"
"TrigToExp" "TrimmedMean" "TrimmedVariance" "TropicalStormData" "True"
"TrueQ" "TruncatedDistribution" "TruncatedPolyhedron"
"TsallisQExponentialDistribution" "TsallisQGaussianDistribution"
"TTest" "Tube" "TubeBezierCurveBox" "TubeBezierCurveBoxOptions"
"TubeBox" "TubeBoxOptions" "TubeBSplineCurveBox"
"TubeBSplineCurveBoxOptions" "Tuesday" "TukeyLambdaDistribution"
"TukeyWindow" "TunnelData" "Tuples" "TuranGraph" "TuringMachine"
"TuttePolynomial" "TwoWayRule" "Typed" "TypeSpecifier"
"UnateQ" "Uncompress" "UnconstrainedParameters" "Undefined"
"UnderBar" "Underflow" "Underlined" "Underoverscript"
"UnderoverscriptBox" "UnderoverscriptBoxOptions" "Underscript"
"UnderscriptBox" "UnderscriptBoxOptions" "UnderseaFeatureData"
"UndirectedEdge" "UndirectedGraph" "UndirectedGraphQ" "UndoOptions"
"UndoTrackedVariables" "Unequal" "UnequalTo" "Unevaluated"
"UniformDistribution" "UniformGraphDistribution" "UniformPolyhedron"
"UniformSumDistribution" "Uninstall" "Union" "UnionedEntityClass"
"UnionPlus" "Unique" "UnitaryMatrixQ" "UnitBox" "UnitConvert"
"UnitDimensions" "Unitize" "UnitRootTest" "UnitSimplify" "UnitStep"
"UnitSystem" "UnitTriangle" "UnitVector" "UnitVectorLayer"
"UnityDimensions" "UniverseModelData" "UniversityData" "UnixTime"
"Unprotect" "UnregisterExternalEvaluator" "UnsameQ" "UnsavedVariables"
"Unset" "UnsetShared" "UntrackedVariables" "Up" "UpArrow" "UpArrowBar"
"UpArrowDownArrow" "Update" "UpdateDynamicObjects"
"UpdateDynamicObjectsSynchronous" "UpdateInterval" "UpdatePacletSites"
"UpdateSearchIndex" "UpDownArrow" "UpEquilibrium" "UpperCaseQ"
"UpperLeftArrow" "UpperRightArrow" "UpperTriangularize"
"UpperTriangularMatrixQ" "Upsample" "UpSet" "UpSetDelayed" "UpTee"
"UpTeeArrow" "UpTo" "UpValues" "URL" "URLBuild" "URLDecode"
"URLDispatcher" "URLDownload" "URLDownloadSubmit" "URLEncode"
"URLExecute" "URLExpand" "URLFetch" "URLFetchAsynchronous" "URLParse"
"URLQueryDecode" "URLQueryEncode" "URLRead" "URLResponseTime"
"URLSave" "URLSaveAsynchronous" "URLShorten" "URLSubmit"
"UseGraphicsRange" "UserDefinedWavelet" "Using" "UsingFrontEnd"
"UtilityFunction" "V2Get" "ValenceErrorHandling" "ValenceFilling"
"ValidationLength" "ValidationSet" "Value" "ValueBox"
"ValueBoxOptions" "ValueDimensions" "ValueForm"
"ValuePreprocessingFunction" "ValueQ" "Values" "ValuesData"
"Variables" "Variance" "VarianceEquivalenceTest"
"VarianceEstimatorFunction" "VarianceGammaDistribution"
"VarianceGammaPointProcess" "VarianceTest" "VectorAngle"
"VectorAround" "VectorAspectRatio" "VectorColorFunction"
"VectorColorFunctionScaling" "VectorDensityPlot" "VectorGlyphData"
"VectorGreater" "VectorGreaterEqual" "VectorLess" "VectorLessEqual"
"VectorMarkers" "VectorPlot" "VectorPlot3D" "VectorPoints" "VectorQ"
"VectorRange" "Vectors" "VectorScale" "VectorScaling" "VectorSizes"
"VectorStyle" "Vee" "Verbatim" "Verbose"
"VerboseConvertToPostScriptPacket" "VerificationTest"
"VerifyConvergence" "VerifyDerivedKey" "VerifyDigitalSignature"
"VerifyFileSignature" "VerifyInterpretation"
"VerifySecurityCertificates" "VerifySolutions" "VerifyTestAssumptions"
"Version" "VersionedPreferences" "VersionNumber" "VertexAdd"
"VertexCapacity" "VertexColors" "VertexComponent" "VertexConnectivity"
"VertexContract" "VertexCoordinateRules" "VertexCoordinates"
"VertexCorrelationSimilarity" "VertexCosineSimilarity" "VertexCount"
"VertexCoverQ" "VertexDataCoordinates" "VertexDegree" "VertexDelete"
"VertexDiceSimilarity" "VertexEccentricity" "VertexInComponent"
"VertexInDegree" "VertexIndex" "VertexJaccardSimilarity"
"VertexLabeling" "VertexLabels" "VertexLabelStyle" "VertexList"
"VertexNormals" "VertexOutComponent" "VertexOutDegree" "VertexQ"
"VertexRenderingFunction" "VertexReplace" "VertexShape"
"VertexShapeFunction" "VertexSize" "VertexStyle"
"VertexTextureCoordinates" "VertexWeight" "VertexWeightedGraphQ"
"Vertical" "VerticalBar" "VerticalForm" "VerticalGauge"
"VerticalSeparator" "VerticalSlider" "VerticalTilde" "Video"
"VideoCombine" "VideoDelete" "VideoEncoding" "VideoExtractFrames"
"VideoFrameList" "VideoFrameMap" "VideoGenerator" "VideoIntervals"
"VideoJoin" "VideoMap" "VideoMapList" "VideoMapTimeSeries"
"VideoPause" "VideoPlay" "VideoQ" "VideoSplit" "VideoStop"
"VideoStream" "VideoStreams" "VideoTrackSelection" "VideoTranscode"
"VideoTrim" "ViewAngle" "ViewCenter" "ViewMatrix" "ViewPoint"
"ViewPointSelectorSettings" "ViewPort" "ViewProjection" "ViewRange"
"ViewVector" "ViewVertical" "VirtualGroupData" "Visible" "VisibleCell"
"VoiceStyleData" "VoigtDistribution" "VolcanoData"
"Volume" "VonMisesDistribution" "VoronoiMesh" "WaitAll"
"WaitAsynchronousTask" "WaitNext" "WaitUntil" "WakebyDistribution"
"WalleniusHypergeometricDistribution" "WaringYuleDistribution"
"WarpingCorrespondence" "WarpingDistance" "WatershedComponents"
"WatsonUSquareTest" "WattsStrogatzGraphDistribution"
"WaveletBestBasis" "WaveletFilterCoefficients" "WaveletImagePlot"
"WaveletListPlot" "WaveletMapIndexed" "WaveletMatrixPlot" "WaveletPhi"
"WaveletPsi" "WaveletScale" "WaveletScalogram" "WaveletThreshold"
"WavePDEComponent" "WeaklyConnectedComponents"
"WeaklyConnectedGraphComponents" "WeaklyConnectedGraphQ"
"WeakStationarity" "WeatherData" "WeatherForecastData"
"WebAudioSearch" "WebElementObject" "WeberE" "WebExecute" "WebImage"
"WebImageSearch" "WebSearch" "WebSessionObject" "WebSessions"
"WebWindowObject" "Wedge" "Wednesday" "WeibullDistribution"
"WeierstrassE1" "WeierstrassE2" "WeierstrassE3" "WeierstrassEta1"
"WeierstrassEta2" "WeierstrassEta3" "WeierstrassHalfPeriods"
"WeierstrassHalfPeriodW1" "WeierstrassHalfPeriodW2"
"WeierstrassHalfPeriodW3" "WeierstrassInvariantG2"
"WeierstrassInvariantG3" "WeierstrassInvariants" "WeierstrassP"
"WeierstrassPPrime" "WeierstrassSigma" "WeierstrassZeta"
"WeightedAdjacencyGraph" "WeightedAdjacencyMatrix" "WeightedData"
"WeightedGraphQ" "Weights" "WelchWindow" "WheelGraph" "WhenEvent"
"Which" "While" "White" "WhiteNoiseProcess" "WhitePoint" "Whitespace"
"WhitespaceCharacter" "WhittakerM" "WhittakerW" "WienerFilter"
"WienerProcess" "WignerD" "WignerSemicircleDistribution"
"WikidataData" "WikidataSearch" "WikipediaData" "WikipediaSearch"
"WilksW" "WilksWTest" "WindDirectionData" "WindingCount"
"WindingPolygon" "WindowClickSelect" "WindowElements" "WindowFloating"
"WindowFrame" "WindowFrameElements" "WindowMargins" "WindowMovable"
"WindowOpacity" "WindowPersistentStyles" "WindowSelected" "WindowSize"
"WindowStatusArea" "WindowTitle" "WindowToolbars" "WindowWidth"
"WindSpeedData" "WindVectorData" "WinsorizedMean" "WinsorizedVariance"
"WishartMatrixDistribution" "With" "WithCleanup" "WolframAlpha"
"WolframAlphaDate" "WolframAlphaQuantity" "WolframAlphaResult"
"WolframLanguageData" "Word" "WordBoundary" "WordCharacter"
"WordCloud" "WordCount" "WordCounts" "WordData" "WordDefinition"
"WordFrequency" "WordFrequencyData" "WordList" "WordOrientation"
"WordSearch" "WordSelectionFunction" "WordSeparators" "WordSpacings"
"WordStem" "WordTranslation" "WorkingPrecision" "WrapAround" "Write"
"WriteLine" "WriteString" "Wronskian" "XMLElement" "XMLObject"
"XMLTemplate" "Xnor" "Xor" "XYZColor" "Yellow" "Yesterday"
"YuleDissimilarity" "ZernikeR" "ZeroSymmetric" "ZeroTest"
"ZeroWidthTimes" "Zeta" "ZetaZero" "ZIPCodeData" "ZipfDistribution"
"ZoomCenter" "ZoomFactor" "ZTest" "ZTransform" ))

(defvar xah-wolfram-dollar-names nil "List of Wolfram Language symbols. Part of many.")

(setq xah-wolfram-dollar-names
'("$Aborted"
"$ActivationGroupID" "$ActivationKey" "$ActivationUserRegistered"
"$AddOnsDirectory" "$AllowDataUpdates"
"$AllowExternalChannelFunctions" "$AllowInternet" "$AssertFunction"
"$Assumptions" "$AsynchronousTask" "$AudioDecoders" "$AudioEncoders"
"$AudioInputDevices" "$AudioOutputDevices" "$BaseDirectory"
"$BasePacletsDirectory" "$BatchInput" "$BatchOutput" "$BlockchainBase"
"$BoxForms" "$ByteOrdering" "$CacheBaseDirectory" "$Canceled"
"$ChannelBase" "$CharacterEncoding" "$CharacterEncodings"
"$CloudAccountName" "$CloudBase" "$CloudConnected" "$CloudConnection"
"$CloudCreditsAvailable" "$CloudEvaluation" "$CloudExpressionBase"
"$CloudObjectNameFormat" "$CloudObjectURLType" "$CloudRootDirectory"
"$CloudSymbolBase" "$CloudUserID" "$CloudUserUUID" "$CloudVersion"
"$CloudVersionNumber" "$CloudWolframEngineVersionNumber"
"$CommandLine" "$CompilationTarget" "$ConditionHold"
"$ConfiguredKernels" "$Context" "$ContextPath" "$ControlActiveSetting"
"$Cookies" "$CookieStore" "$CreationDate"
"$CryptographicEllipticCurveNames" "$CurrentLink" "$CurrentTask"
"$CurrentWebSession" "$DatabaseAuthentications" "$Databases"
"$DataStructures" "$DateStringFormat" "$DBResourceObject"
"$DefaultAudioInputDevice" "$DefaultAudioOutputDevice" "$DefaultFont"
"$DefaultFrontEnd" "$DefaultImagingDevice" "$DefaultLocalBase"
"$DefaultMailbox" "$DefaultNetworkInterface" "$DefaultPath"
"$DefaultProxyRules" "$DefaultRemoteBatchSubmissionEnvironment"
"$DefaultRemoteKernel" "$DefaultSystemCredentialStore"
"$devTextSearchOutput" "$Display" "$DisplayFunction"
"$DistributedContexts" "$DynamicEvaluation" "$Echo"
"$EmbedCodeEnvironments" "$EmbeddableServices" "$EntityStores"
"$Epilog" "$EvaluationCloudBase" "$EvaluationCloudObject"
"$EvaluationEnvironment" "$ExportFormats" "$ExternalIdentifierTypes"
"$ExternalStorageBase" "$Failed" "$FinancialDataSource"
"$FontFamilies" "$FormatType" "$FrontEnd" "$FrontEndSession"
"$GeneratedAssetLocation" "$GeoEntityTypes" "$GeoLocation"
"$GeoLocationCity" "$GeoLocationCountry" "$GeoLocationPrecision"
"$GeoLocationSource" "$HistoryLength" "$HomeDirectory"
"$HTMLExportRules" "$HTTPCookies" "$HTTPRequest" "$IgnoreEOF"
"$ImageFormattingWidth" "$ImageResolution" "$ImagingDevice"
"$ImagingDevices" "$ImportFormats" "$IncomingMailSettings"
"$InitialDirectory" "$Initialization" "$InitializationContexts"
"$Input" "$InputFileName" "$InputStreamMethods" "$Inspector"
"$InstallationDate" "$InstallationDirectory" "$InterfaceEnvironment"
"$InterpreterTypes" "$IterationLimit" "$KernelCount" "$KernelID"
"$Language" "$LaunchDirectory" "$LibraryPath" "$LicenseExpirationDate"
"$LicenseID" "$LicenseProcesses" "$LicenseServer"
"$LicenseSubprocesses" "$LicenseType" "$Line" "$Linked"
"$LinkSupported" "$LoadedFiles" "$LocalBase" "$LocalSymbolBase"
"$MachineAddresses" "$MachineDomain" "$MachineDomains"
"$MachineEpsilon" "$MachineID" "$MachineName" "$MachinePrecision"
"$MachineType" "$MaxExtraPrecision" "$MaxLicenseProcesses"
"$MaxLicenseSubprocesses" "$MaxMachineNumber" "$MaxNumber"
"$MaxPiecewiseCases" "$MaxPrecision" "$MaxRootDegree" "$MessageGroups"
"$MessageList" "$MessagePrePrint" "$Messages" "$MinMachineNumber"
"$MinNumber" "$MinorReleaseNumber" "$MinPrecision" "$MobilePhone"
"$ModuleNumber" "$NetworkConnected" "$NetworkInterfaces"
"$NetworkLicense" "$NewMessage" "$NewOptimizations" "$NewSymbol"
"$NotebookInlineStorageLimit" "$Notebooks" "$NoValue" "$NumberMarks"
"$NumberOfExtraPages" "$Off" "$OperatingSystem" "$Output"
"$OutputForms" "$OutputSizeLimit" "$OutputStreamMethods" "$Packages"
"$ParentLink" "$ParentProcessID" "$PasswordFile" "$PatchLevelID"
"$Path" "$PathnameSeparator" "$PerformanceGoal" "$Permissions"
"$PermissionsGroupBase" "$PersistenceBase" "$PersistencePath"
"$PipeSupported" "$PlotTheme" "$Post" "$Pre" "$PreferencesDirectory"
"$PreInitialization" "$PrePrint" "$PreRead" "$PrintForms"
"$PrintLiteral" "$Printout3DPreviewer" "$ProcessID" "$ProcessorCount"
"$ProcessorType" "$ProductInformation" "$ProgramName" "$PublisherID"
"$RandomGeneratorState" "$RandomState" "$RecursionLimit"
"$RegisteredDeviceClasses" "$RegisteredUserName" "$ReleaseNumber"
"$RequesterAddress" "$RequesterCloudUserID" "$RequesterCloudUserUUID"
"$RequesterWolframID" "$RequesterWolframUUID" "$ResourceSystemBase"
"$ResourceSystemPath" "$RootDirectory" "$ScheduledTask"
"$ScriptCommandLine" "$ScriptInputString" "$SearchLanguage"
"$SecuredAuthenticationKeyTokens" "$ServiceCreditsAvailable"
"$Services" "$SessionID" "$SetParentLink" "$SharedFunctions"
"$SharedVariables" "$SoundDisplay" "$SoundDisplayFunction"
"$SourceLink" "$SSHAuthentication" "$SubtitleDecoders"
"$SubtitleEncoders" "$SummaryBoxDataSizeLimit"
"$SuppressInputFormHeads" "$SynchronousEvaluation" "$SyntaxHandler"
"$System" "$SystemCharacterEncoding" "$SystemCredentialStore"
"$SystemID" "$SystemMemory" "$SystemShell" "$SystemTimeZone"
"$SystemWordLength" "$TemplatePath" "$TemporaryDirectory"
"$TemporaryPrefix" "$TestFileName" "$TextStyle" "$TimedOut"
"$TimeUnit" "$TimeZone" "$TimeZoneEntity" "$TopDirectory" "$TraceOff"
"$TraceOn" "$TracePattern" "$TracePostAction" "$TracePreAction"
"$TriangleInstallationDirectory" "$TriangleLibrary" "$UnitSystem"
"$Urgent" "$UserAddOnsDirectory" "$UserAgentLanguages"
"$UserAgentMachine" "$UserAgentName" "$UserAgentOperatingSystem"
"$UserAgentString" "$UserAgentVersion" "$UserBaseDirectory"
"$UserBasePacletsDirectory" "$UserDocumentsDirectory" "$Username"
"$UserName" "$UserURLBase" "$Version" "$VersionNumber"
"$VideoDecoders" "$VideoEncoders" "$VoiceStyles"
"$WolframDocumentsDirectory" "$WolframID" "$WolframUUID" )
)

(defvar xah-wolfram-all-symbols nil "List of all Wolfram Language symbols.")

(setq xah-wolfram-all-symbols
      (append
       xah-wolfram-funs1
       xah-wolfram-funs2
       xah-wolfram-funs2-5
       xah-wolfram-funs3
       xah-wolfram-funs3-5
       xah-wolfram-funs4
       xah-wolfram-dollar-names
       ))



(defun xah-wolfram-doc-lookup ()
  "Look up the symbol under cursor in Wolfram doc site in web browser.
Version: 2021-07-25 2021-09-15 2021-09-20"
  (interactive)
  (let* ((xword
          (if (region-active-p)
              (buffer-substring-no-properties (region-beginning) (region-end))
            (upcase-initials (current-word))))
         (xurl (format "https://reference.wolfram.com/language/ref/%s.html" xword)))
    (browse-url xurl)))

(defun xah-wolfram-delete-comment-backward ()
  "Delete comment to the left of cursor.
cursor must be at end of comment, like this
(* comment *)▮
Version: 2023-11-12 2023-12-12"
  (interactive)
  (let ((xp0 (point)))
    ;; 41 is right paren, 42 is asterisk
    (when (and (eq (char-before) 41) (eq (char-before (1- xp0)) ?*))
      (search-backward "(*")
      (kill-region (point) xp0))))

(defun xah-wolfram-smart-delete-backward ()
  "Delete a thing to the left of cursor.
If cursor left is comment bracket e.g. 「(▮* comment *)▮」, delete whole comment.
If cursor left is a string e.g. 「\"▮some\"▮」, delete whole string.
If cursor left is a bracket e.g. 「[▮some]▮」, delete whole bracketed text.
Else, just delete backward 1 char.
Deleted text can be pasted later (except 1 char).

When `universal-argument' is called first, do not delete the inner text.

Version: 2023-11-12 2023-12-12"
  (interactive)
  (let ((xp0 (point)))
    (cond
     ;; inside comment
     ((nth 4 (syntax-ppss)) (delete-char -1))

     ;; handle comment case when cursor at end, e.g. *)▮
     ;; 41 is right paren
     ((and (eq (char-before) 41) (eq (char-before (1- xp0)) ?*))
      (xah-wolfram-delete-comment-backward))

     ;; handle comment case when cursor is at (▮*
     ;; 40 is left paren
     ((and (eq (char-before) 40) (eq (char-after) ?*))
      (search-forward "*)")
      (xah-wolfram-delete-comment-backward))

     ;; left is closing bracket
     ((prog2 (backward-char) (looking-at "\\s)") (forward-char))
      (let (xp1)
        (forward-sexp -1)
        (setq xp1 (point))
        (if current-prefix-arg
            (progn
              (goto-char xp0)
              (delete-char -1)
              (push-mark (point))
              (goto-char xp1)
              (delete-char 1))
          (progn
            (kill-region xp1 xp0)))))

     ;; left is opening bracket
     ((prog2 (backward-char) (looking-at "\\s(") (forward-char))
      (let (xp1 xp2)
        (backward-char)
        (setq xp1 (point))
        (forward-sexp)
        (setq xp2 (point))
        (if current-prefix-arg
            (progn
              (goto-char xp2)
              (delete-char -1)
              (push-mark (point))
              (goto-char xp1)
              (delete-char 1))
          (progn
            (kill-region xp1 xp2)))))

     ;; handle string case
     ((prog2 (backward-char) (looking-at "\\s\"") (forward-char))
      (let (xp1 xp2)
        ;; xp1 xp2 are the begin and end pos of the string
        ;; if inside string
        (if (nth 3 (syntax-ppss))
            (setq xp1 (1- xp0)
                  xp2
                  (progn
                    (backward-char)
                    (forward-sexp)
                    (point)))
          (setq xp2 (point)
                xp1
                (progn (forward-sexp -1) (point))))
        (if current-prefix-arg
            (progn
              (message "prefix case")
              (goto-char xp2)
              (delete-char -1)
              (goto-char xp1)
              (delete-char 1))
          (kill-region xp1 xp2))))
     (t (delete-char -1)))))

(defun xah-wolfram-smart-newline ()
  "Insert a semicolon and a newline.
Version: 2021-08-06"
  (interactive)
  (insert ";\n"))


;; abbrev

(defun xah-wolfram-abbrev-enable-function ()
  "Return t if not in string or comment. Else nil.
This is for abbrev table property `:enable-function'.
Version: 2021-07-24"
  (let ((xsyntaxState (syntax-ppss)))
    (not (or (nth 3 xsyntaxState) (nth 4 xsyntaxState)))))

(defun xah-wolfram-expand-abbrev ()
  "Function for value of variable `abbrev-expand-function'.
Expand the symbol before cursor, if cursor is not in string or comment.
Returns the abbrev symbol if there's a expansion, else nil.
Version: 2021-07-24"
  (interactive)
  (when (xah-wolfram-abbrev-enable-function) ; abbrev property :enable-function doesn't seem to work, so check here instead
    (let ( xp1 xp2 xabrStr xabrSymbol )
      ;; (save-excursion
      ;;   (forward-symbol -1)
      ;;   (setq xp1 (point))
      ;;   (goto-char xp0)
      ;;   (setq xp2 xp0))
      (save-excursion
        ;; 2017-01-16 note: we select the whole symbol to solve a problem. problem is: if “aa”  is a abbrev, and “▮bbcc” is existing word with cursor at beginning, and user wants to type aa-bbcc. Normally, aa immediately expands. This prevent people editing bbcc to become aa-bbcc. This happens for example in elisp “search-forward” to get “re-search-forward”. The downside of this is that, people cannot type a abbrev when in middle of a word.
        (forward-symbol -1)
        (setq xp1 (point))
        (forward-symbol 1)
        (setq xp2 (point)))
      (setq xabrStr (buffer-substring-no-properties xp1 xp2)
            xabrSymbol (abbrev-symbol xabrStr))
      (if xabrSymbol
          (progn
            (abbrev-insert xabrSymbol xabrStr xp1 xp2 )
            (xah-wolfram--abbrev-position-cursor xp1)
            xabrSymbol)
        nil))))

(defun xah-wolfram--abbrev-position-cursor (&optional Pos)
  "Move cursor back to ▮ if exist.
Pos is a buffer position limit of search backward. If nil, default to point minus 100.
Return point if found, else nil.
Version: 2016-10-24 2023-08-20"
  (interactive)
  (let ((xfoundQ (search-backward "▮" (if Pos Pos (max (point-min) (- (point) 100))) t )))
    (when xfoundQ (delete-char 1))
    xfoundQ
    ))

(defun xah-wolfram--abhook ()
  "Abbrev hook function, used for `define-abbrev'.
 Our use is to prevent inserting the char that triggered expansion. Experimental.
Version: 2016-10-24"
  t)

(put 'xah-wolfram--abhook 'no-self-insert t)

(defvar xah-wolfram-mode-abbrev-table nil "abbrev table" )

(setq xah-wolfram-mode-abbrev-table nil)

(define-abbrev-table 'xah-wolfram-mode-abbrev-table
  '(

    ;; abbrevs that expand to syntax

    ("eq" "== " xah-wolfram--abhook)
    ("eqq" "=== " xah-wolfram--abhook)
    ("fn" "((#▮) &)" xah-wolfram--abhook)
    ("m" "/@ " xah-wolfram--abhook)
    ("pt" " //Print" xah-wolfram--abhook)
    ("ra" "-> ▮" xah-wolfram--abhook)
    ("same" "=== " xah-wolfram--abhook)
    ("set" "= " xah-wolfram--abhook)
    ("at" "@ " xah-wolfram--abhook)
    ("var" "x = 3;" xah-wolfram--abhook)

    ;; common abbrevs

    ("acos" "ArcCos" xah-wolfram--abhook)
    ("asin" "ArcSin" xah-wolfram--abhook)
    ("atan" "ArcTan" xah-wolfram--abhook)
    ("clr" "Clear[▮];" xah-wolfram--abhook)
    ("deg" "Degree" xah-wolfram--abhook)
    ("inf" "Infinity" xah-wolfram--abhook)
    ("len" "Length" xah-wolfram--abhook)
    ("lim" "Limit" xah-wolfram--abhook)

    ;; odd abbrevs

    ("1st" "First[▮]" xah-wolfram--abhook)
    ("last" "Last[▮]" xah-wolfram--abhook)
    ("asso" "Association" xah-wolfram--abhook)
    ("ff" "FullForm" xah-wolfram--abhook)
    ("fun" "Function" xah-wolfram--abhook)
    ("get" "Get[▮]" xah-wolfram--abhook)
    ("g3d" "Graphics3D" xah-wolfram--abhook)
    ("gc" "GraphicsComplex[▮,]" xah-wolfram--abhook)
    ("gra" "Graphics" xah-wolfram--abhook)
    ("md" "Module" xah-wolfram--abhook)
    ("p" "Print" xah-wolfram--abhook)
    ("pp3" "ParametricPlot3D" xah-wolfram--abhook)
    ("pp3d" "ParametricPlot3D"  xah-wolfram--abhook)
    ("pr" "PlotRange" xah-wolfram--abhook)
    ("prod" "Product" xah-wolfram--abhook)
    ("regex" "RegularExpression" xah-wolfram--abhook)
    ("sl" "StringLength" xah-wolfram--abhook)

    ;; function templates

    ("Association" "Association[▮ a -> 1, b -> 2]" xah-wolfram--abhook)
    ("Function" "Function[{x},▮expr]" xah-wolfram--abhook)
    ("GeometricTransformation" "GeometricTransformation[▮,tf]" xah-wolfram--abhook)
    ("Graphics" "Graphics[▮, Axes -> True ]" xah-wolfram--abhook)
    ("Graphics3D" "Graphics3D[▮, Axes -> True ]" xah-wolfram--abhook)
    ("If" "If[▮,y,n]" xah-wolfram--abhook)
    ("Limit" "Limit[x▮ , {x -> Infinity }]" xah-wolfram--abhook)
    ("Map" "Map[▮, list,{1}]" xah-wolfram--abhook)
    ("Module" "Module[{x=2▮},\nexpr]" xah-wolfram--abhook)
    ("ParametricPlot3D" "ParametricPlot3D[\n{Cos[u]*(2 + 1*Cos[v]), Sin[u]*(2 + 1*Cos[v]), 1*Sin[v]} , \n{u, 0, 6}, \n{v, 0, 6}, \n PlotPoints -> 100,\n Axes -> True,\n Boxed -> True,\n BoundaryStyle -> Directive[Black, Thin],\n PlotStyle -> Directive[White, Opacity[0.7], Specularity[10, 20]],\n Lighting -> \"Neutral\"]\n\n" xah-wolfram--abhook)
    ("Plot" "Plot[ Sin[x], {x, 1, 9}]" xah-wolfram--abhook)
    ("PlotRange" "PlotRange->{9▮}" xah-wolfram--abhook)
    ("RegularExpression" "RegularExpression[\"▮\"]" xah-wolfram--abhook)
    ("Table" "Table[ ▮, {x, 1, 9}]" xah-wolfram--abhook)
    ("With" "With[{x=2▮}, expr]" xah-wolfram--abhook)

    ;;
    )

  "Abbrev table for `xah-wolfram-mode'"
  :case-fixed t
  :system t
  :enable-function 'xah-wolfram-abbrev-enable-function
  )

;; generate abbrev. simple lowercase to cap
(mapc
 (lambda (x)
   (define-abbrev
     xah-wolfram-mode-abbrev-table x
     (upcase-initials x)
     'xah-wolfram--abhook))
 '(

   "pi"

   "if"
   "map"
   "module"
   "with"
   "plot"
   "range"
   "table"

   "sin"
   "cos"
   "tan"

   "power"
   "product"
   "sum"

   "true"
   "false"

   ))

;; generate abbrev for simple template of functions. just add [▮]
(mapc
 (lambda (x)
   (define-abbrev xah-wolfram-mode-abbrev-table x (concat x "[▮]") 'xah-wolfram--abhook))
 '(
   "Sin"
   "Cos"
   "Tan"
   "ArcSin"
   "ArcCos"
   "ArcTan"
   ))


;; indent/reformat related

(defun xah-wolfram-complete-or-indent ()
  "Do keyword completion or pretty format code.
If cursor is in string or comment, do prettify.
If char before cursor is a word, do completion, else do prettify.
Prettify is done by `xah-wolfram-format-pretty'.

Version: 2023-07-22 2023-08-02"
  (interactive)
  (let ((xsyntaxState (syntax-ppss)))
    (cond
     ((or (nth 3 xsyntaxState) (nth 4 xsyntaxState))
      (xah-wolfram-format-pretty))
     ((eq (point-min) (point))
      (xah-wolfram-format-pretty))
     ((and
       (prog2 (backward-char) (looking-at "[a-zA-Z]") (forward-char))
       (or (eobp) (looking-at "[\n[:blank:][:punct:]]")))
      (xah-wolfram-complete-symbol))
     (t (xah-wolfram-format-pretty)))))

(defun xah-wolfram-replace-special-char ()
  "Replace \\[Pi] by Pi in current text block.
Version: 2024-02-07"
  (interactive)
  (save-excursion
    (let (xp1 xp2)
      (setq xp1 (if (search-backward "\n\n" nil t) (+ (point) 2) (point-min))
            xp2 (if (search-forward "\n\n" nil t) (- (point) 2) (point-max)))
      (save-restriction
        (narrow-to-region xp1 xp2)
        (let ((case-fold-search nil))
          (goto-char (point-min)) (while (search-forward "\\[Pi]" nil t) (replace-match "Pi")))))))

(defun xah-wolfram-format-compact ()
  "Format current block in compact style.
Version: 2021-08-01 2023-10-13 2024-03-24"
  (interactive)
  (let (xp1 xp2)
    (seq-setq (xp1 xp2) (xah-get-pos-of-block-or))
    (save-restriction
      (narrow-to-region xp1 xp2)
      (xah-wolfram-replace-regexp-pairs-region
       (point-min) (point-max)
       [
        ["\\([A-Za-z0-9]\\) @" "\\1@"]
        ] t nil t)
      (xah-wolfram-replace-regexp-pairs-region
       (point-min) (point-max)
       [
        ["  " " "]
        [" \\[" "["]
        ["\\[ " "["]
        [" \\]" "]"]
        [" =" "="]
        ["= " "="]
        [" &" "&"]
        [" /@ " "/@"]
        [" -> " "->"]
        [" :> " ":>"]
        [" := " ":="]
        [" , " ","]
        [", " ","]
        [" ;" ";"]
        ]
       ))))

(defun xah-wolfram-format-pretty ()
  "Format current block in readable style.
Version: 2021-07-25 2023-10-13 2024-03-24"
  (interactive)
  (let (xp1 xp2)
    (seq-setq (xp1 xp2) (xah-get-pos-of-block-or))
    (save-restriction
      (narrow-to-region xp1 xp2)
      (xah-wolfram-replace-regexp-pairs-region
       (point-min) (point-max)
       [
        ;; before comma
        ["\\([]}A-Za-z0-9]\\) ," "\\1,"]
        ;; after comma
        [",\\([#({A-Za-z0-9]\\)" ", \\1"]

        ;; before plus
        ["\\([A-Za-z0-9]\\)\\+" "\\1 +"]
        ;; after plus
        ["\\+\\([(A-Za-z0-9]\\)" "+ \\1"]

        ;; before equal
        ["\\([A-Za-z0-9]\\)=" "\\1 ="]
        ;; after equal
        ["=\\([(A-Za-z0-9]\\)" "= \\1"]

        ;; before colon equal
        ["\\(\\]\\):=" "\\1 :="]
        ;; after colon equal
        [":=\\(\\]\\)" ":= \\1"]

        ;; before triple equal
        ["\\([]A-Za-z0-9]\\) *===" "\\1 ==="]
        ;; after triple equal
        ["===\\([(A-Za-z0-9]\\)" " === \\1"]

        ;; before double equal
        ["\\([]A-Za-z0-9]\\)==" "\\1 =="]
        ;; after double equal
        ["==\\([(A-Za-z0-9]\\)" " == \\1"]

        ;; ["\\([A-Za-z0-9]\\):=" "\\1 :="]
        ;; [":=\\([(A-Za-z0-9]\\)" ":= \\1"]

        ;; before slash at
        ["\\([)&A-Za-z]\\)/@" "\\1 /@"]
        ;; after slash at
        ["/@\\([(A-Za-z]\\)" "/@ \\1"]

        ["\n;" ";"]
        ] t)
      (xah-wolfram-replace-regexp-pairs-region
       (point-min) (point-max)
       [
        ;; [" := " ":="]
        ["]:>" "] :>"]
        ["\n\n\n+" "\n\n"]
        ]))))


;; completion

(defun xah-wolfram-complete-symbol ()
  "Perform keyword completion on current symbol.

Version: 2017-01-27 2023-02-12 2023-09-29"
  (interactive)
  (let ((xp0 (point)) xp1 xp2 xthisSym xresultSym)
    (save-excursion
      (skip-chars-backward "$A-Za-z0-9")
      (setq xp1 (point))
      (goto-char xp0)
      (skip-chars-forward "$A-Za-z0-9")
      (setq xp2 (point)))
    (setq xthisSym (buffer-substring-no-properties xp1 xp2))
    (setq xresultSym (completing-read "" xah-wolfram-all-symbols nil t xthisSym))
    (delete-region xp1 xp2)
    (goto-char xp1)
    (insert xresultSym "[  ]")
    (backward-char 2)))


;; syntax table
(defvar xah-wolfram-mode-syntax-table nil "Syntax table for `xah-wolfram-mode'.")

(setq
 xah-wolfram-mode-syntax-table
 (let ((xsynTable (make-syntax-table )))

   ;; (modify-syntax-entry ?\( "()" xsynTable)
   ;; (modify-syntax-entry ?\) ")(" xsynTable)
   ;; (modify-syntax-entry ?\[ "(]" xsynTable)
   ;; (modify-syntax-entry ?\] ")[" xsynTable)
   ;; (modify-syntax-entry ?\{ "(}" xsynTable)
   ;; (modify-syntax-entry ?\} "){" xsynTable)

   ;; comment
   (modify-syntax-entry ?\( "()1n" xsynTable)
   (modify-syntax-entry ?\) ")(4n" xsynTable)
   (modify-syntax-entry ?* ". 23n" xsynTable)

   ;; symbol
   (modify-syntax-entry ?$ "_" xsynTable)

   ;; punctuation
   (modify-syntax-entry ?! "." xsynTable)
   (modify-syntax-entry ?# "." xsynTable)
   (modify-syntax-entry ?% "." xsynTable)
   (modify-syntax-entry ?& "." xsynTable)
   (modify-syntax-entry ?' "." xsynTable)
   (modify-syntax-entry ?+ "." xsynTable)
   (modify-syntax-entry ?, "." xsynTable)
   (modify-syntax-entry ?- "." xsynTable)
   (modify-syntax-entry ?. "." xsynTable)
   (modify-syntax-entry ?/ "." xsynTable)
   (modify-syntax-entry ?: "." xsynTable)
   (modify-syntax-entry ?\; "." xsynTable)
   (modify-syntax-entry ?< "." xsynTable)
   (modify-syntax-entry ?= "." xsynTable)
   (modify-syntax-entry ?> "." xsynTable)
   (modify-syntax-entry ?? "." xsynTable)
   (modify-syntax-entry ?@ "." xsynTable)
   (modify-syntax-entry ?\ "." xsynTable)
   (modify-syntax-entry ?^ "." xsynTable)
   (modify-syntax-entry ?_ "." xsynTable)
   (modify-syntax-entry ?` "." xsynTable)
   (modify-syntax-entry ?| "." xsynTable)
   (modify-syntax-entry ?~ "." xsynTable)

   xsynTable))


;; syntax coloring related

(defface xah-wolfram-var-name
  '((t :foreground "#2e8b57" :weight bold ))
  "face for user variables."
  :group 'xah-wolfram-mode )

(face-spec-set
 'xah-wolfram-var-name
 '((t :foreground "#2e8b57" :weight bold )))

(defvar xah-wolfram-font-lock-keywords nil "Value for `font-lock-defaults'")

(setq
 xah-wolfram-font-lock-keywords
 `((,(regexp-opt xah-wolfram-funs1 'symbols) . font-lock-function-name-face)
   (,(regexp-opt xah-wolfram-funs2 'symbols) . font-lock-function-name-face)
   (,(regexp-opt xah-wolfram-funs2-5 'symbols) . font-lock-function-name-face)
   (,(regexp-opt xah-wolfram-funs3 'symbols) . font-lock-function-name-face)
   (,(regexp-opt xah-wolfram-funs3-5 'symbols) . font-lock-function-name-face)
   (,(regexp-opt xah-wolfram-funs4 'symbols) . font-lock-function-name-face)
   (,(regexp-opt xah-wolfram-dollar-names 'symbols) . font-lock-builtin-face)

   ("\\b[a-z]+[0-9]*_+" . 'xah-wolfram-var-name)
   ("#[0-9]" . 'xah-wolfram-var-name)
   ("#+" . 'xah-wolfram-var-name)
   ("\\b[a-z][A-Za-z0-9]*" . font-lock-variable-name-face)
   ("\\b[A-Z][A-Za-z0-9]*" . font-lock-warning-face)))


;; keybinding

(defvar xah-wolfram-mode-map nil "Keybinding for `xah-wolfram-mode'")
(progn
  (setq xah-wolfram-mode-map (make-sparse-keymap))
  (define-prefix-command 'xah-wolfram-leader-map)

  (define-key xah-wolfram-mode-map
              (if (boundp 'xah-major-mode-leader-key)
                  xah-major-mode-leader-key
                (kbd "TAB"))
              xah-wolfram-leader-map)

  ;; (define-key xah-wolfram-leader-map (kbd "SPC") #'xah-wolfram-complete-or-indent)
  (define-key xah-wolfram-leader-map (kbd "SPC") #'xah-wolfram-complete-symbol)

  (define-key xah-wolfram-leader-map (kbd "c") #'xah-wolfram-format-compact)
  (define-key xah-wolfram-leader-map (kbd "d") #'xah-wolfram-smart-delete-backward)

  (define-key xah-wolfram-leader-map (kbd "TAB") #'xah-wolfram-format-pretty)
  (define-key xah-wolfram-leader-map (kbd "a") #'xah-wolfram-run-script-print-all)
  (define-key xah-wolfram-leader-map (kbd "e") #'xah-wolfram-eval-region)
  (define-key xah-wolfram-leader-map (kbd "h") #'xah-wolfram-doc-lookup)
  (define-key xah-wolfram-leader-map (kbd "l") #'xah-wolfram-eval-current-line)
  (define-key xah-wolfram-leader-map (kbd "p") #'xah-wolfram-run-script-print-last)
  (define-key xah-wolfram-leader-map (kbd "r") #'xah-wolfram-run-script)
  (define-key xah-wolfram-leader-map (kbd "t") #'xah-wolfram-replace-special-char)

  (define-key xah-wolfram-leader-map (kbd "<return>") #'xah-wolfram-smart-newline))



;;;###autoload
(define-derived-mode xah-wolfram-mode prog-mode "∑Wolfram"
  "A major mode for Wolfram Language.
\\{xah-wolfram-mode-map}"
  (set-syntax-table xah-wolfram-mode-syntax-table)
  (setq font-lock-defaults '((xah-wolfram-font-lock-keywords)))

  (setq-local comment-start "(*")
  (setq-local comment-end "*)")

  (make-local-variable 'abbrev-expand-function)
  (setq abbrev-expand-function 'xah-wolfram-expand-abbrev)

  ;; (add-hook 'completion-at-point-functions #'xah-wolfram-complete-symbol nil 'local)

  (abbrev-mode 1)

  :group 'xah-wolfram-mode
  )

;;;###autoload
(add-to-list 'auto-mode-alist '("\\.wl\\'" . xah-wolfram-mode))
;;;###autoload
(add-to-list 'auto-mode-alist '("\\.wls\\'" . xah-wolfram-mode))
;;;###autoload
(add-to-list 'auto-mode-alist '("\\.m\\'" . xah-wolfram-mode))
;;;###autoload
(add-to-list 'auto-mode-alist '("\\.nb\\'" . xah-wolfram-mode))

(provide 'xah-wolfram-mode)

;;; xah-wolfram-mode.el ends here
