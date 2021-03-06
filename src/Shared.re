
let fakePhone = try {Sys.getenv("PHONE") |> ignore; true} { | Not_found => false };
let isPhone = Reprocessing.target == "native-ios" || Reprocessing.target == "native-android" || fakePhone;

type context = {
  height: float,
  width: float,
  smallFont: Reprocessing.fontT,
  textFont: Reprocessing.fontT,
  titleFont: Reprocessing.fontT,
  boldTextFont: Reprocessing.fontT,
  smallTitleFont: Reprocessing.fontT,
  charSheet: Reprocessing.imageT,
  itemSheet: Reprocessing.imageT,
  tileSheet: Reprocessing.imageT,
  extraItemsSheet: Reprocessing.imageT,
};

type globalState('status) = {
  context,
  screenState: 'status,
};