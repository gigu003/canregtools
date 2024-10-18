## code to prepare `label_region` dataset goes here

region_label_cn <- c("华北", "东北", "华东", "中南", "西南", "西北", "港澳台")
region_label_en <- c("North", "North East", "East", "South", "South West",
                     "North West", "GAT")
prov_label_cn <- 
  c("北京市", "天津市", "河北省", "山西省", "内蒙古自治区",
    "辽宁省", "吉林省", "黑龙江省",
    "上海市", "江苏省", "浙江省", "安徽省", "福建省", "江西省", "山东省",
    "河南省", "湖北省", "湖南省", "广东省", "广西壮族自治区", "海南省",
    "重庆市", "四川省", "贵州省", "云南省", "西藏自治区",
    "陕西省", "甘肃省", "青海省", "宁夏回族自治区", "新疆维吾尔自治区",
    "台湾省", "香港特别行政区", "澳门特别行政区")
prov_label_en <- 
  c("BeiJing", "TianJin", "HeBei", "ShanXi", "NeiMengGu",
    "LiaoNing", "JiLin", "HeiLongJiang",
    "ShangHai", "JiangSu", "ZheJiang", "AnHui", "FuJian", "JiangXi", "ShanDong",
    "Henan", "HuBei", "HuNan", "GuangDong", "GuangXi", "HaiNan",
    "ChongQing", "SiChuan", "GuiZhou", "YunNan", "XiZang",
    "ShannXi", "GanSu", "QingHai", "NingXia", "XinJiang",
    "TaiWan", "XiangGang", "AoMen")
prov_label <- list(prov_label_cn = prov_label_cn,
                   prov_label_en = prov_label_en)
region_label <- list(region_label_cn = region_label_cn,
                     region_label_en = region_label_en)
