## code to prepare `ethinic` data set goes here
ethnic_map <- data.frame(
  code = sprintf("%02d", c(1:56, 97, 98, 99)),
  cname = c(
    "汉族", "蒙古族", "回族", "藏族", "维吾尔族", "苗族", "彝族", "壮族",
    "布依族", "朝鲜族", "满族", "桐族", "瑶族", "白族", "土家族", "哈尼族",
    "哈萨克族", "傣族", "黎族", "傈僳族", "佤族", "畲族", "高山族", "拉祜族",
    "水族", "东乡族", "纳西族", "景颇族", "柯尔克孜族", "土族", "达斡尔族",
    "仫佬族", "羌族", "布朗族", "撒拉族", "毛难族", "仡佬族", "锡伯族",
    "阿昌族","普米族", "塔吉克族", "怒族", "乌孜别克族", "俄罗斯族",
    "鄂温克族", "德昂族", "保安族", "裕固族", "京族", "塔塔尔族", "独龙族",
    "鄂伦春族", "赫哲族", "门巴族", "珞巴族", "基诺族", "其他",
    "外国血统中国籍人士", "不详"),
  ename = c(
    "Han", "Mongol", "Hui", "Zang", "Uygur", "Miao", "Yi", "Zhuang", "Buyei",
    "Chosen", "Man", "Dong", "Yao", "Bai", "Tujia", "Hani", "Kazak", "Dai",
    "Li", "Lisu", "Va", "She", "Gaoshan", "Lahu", "Sui", "Dongxiang", "Naxi",
    "Jingpo", "Kirgiz", "Tu", "Daur", "Mulao", "Qiang", "Blang", "Salar",
    "Maonan", "Gelao", "Xibe", "Achang", "Pumi", "Tajik", "Nu", "Uzbek",
    "Russ", "Ewenki", "Deang", "Bonan", "Yugur", "Gin", "Tatar", "Derung",
    "Oroqen", "Hezhen", "Monba", "Lhoba", "Jino", "Others", "Foreign",
    "Unknown"),
  abbr = c(
    "HA", "MG", "HU", "ZA", "UG", "MH", "YI", "ZH", "BY", "CS", "MA", "DO",
    "YA", "BA", "TJ", "HN", "KZ", "DA", "LI", "LS", "VA", "SH", "GS", "LH",
    "SU", "DX", "NX", "JP", "KG", "TU", "DU", "ML", "QI", "BL", "SL", "MN",
    "GL", "XB", "AC", "PM", "TA", "NU", "UZ", "RS", "EW", "DE", "BN", "YG",
    "GI", "TT", "DR", "OR", "HZ", "MB", "LB", "JN", "OT", "FO", "MI")
)

