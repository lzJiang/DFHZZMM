FUNCTION zzfg_mm_002_0001.
*"----------------------------------------------------------------------
*"*"本地接口：
*"  IMPORTING
*"     REFERENCE(I_REQ) TYPE  ZZS_MMI002_REQ OPTIONAL
*"  EXPORTING
*"     REFERENCE(O_RESP) TYPE  ZZS_REST_OUT
*"----------------------------------------------------------------------
  " You can use the template 'functionModuleParameter' to add here the signature!
  .
  TYPES: BEGIN OF ty_emailaddress,
           emailaddress TYPE string,
         END OF ty_emailaddress,
         BEGIN OF ty_phonenumber,
           phonenumber TYPE string,
         END OF ty_phonenumber,

         BEGIN OF ty_businesspartneraddres,
           country         TYPE string,
           language        TYPE string,
           streetname      TYPE string,
           to_phonenumber  TYPE TABLE OF ty_phonenumber WITH EMPTY KEY,
           to_emailaddress TYPE TABLE OF ty_emailaddress WITH EMPTY KEY,
         END OF ty_businesspartneraddres,
         BEGIN OF ty_businesspartnertax,
           bptaxtype       TYPE string,
           bptaxlongnumber TYPE string,
         END OF ty_businesspartnertax,
         BEGIN OF ty_businesspartnerrole,
           businesspartnerrole TYPE string,
         END OF ty_businesspartnerrole,
         BEGIN OF ty_partnerfunction,
           partnerfunction TYPE string,
         END OF ty_partnerfunction,
         BEGIN OF ty_salesareatax,
           departurecountry          TYPE string,
           customertaxcategory       TYPE string,
           customertaxclassification TYPE string,
         END OF ty_salesareatax,
         BEGIN OF ty_businesspartnerbank,
           businesspartner          TYPE string,
           bankidentification       TYPE string,
           bankcountrykey           TYPE string,
           banknumber               TYPE string,
           bankaccountholdername    TYPE string,
           bankaccount              TYPE string,
           bankaccountname          TYPE string,
           bankaccountreferencetext TYPE string,
         END OF ty_businesspartnerbank,

         BEGIN OF to_supplierpurchasingorg,
           purchasingorganization     TYPE string,
           currency                   TYPE string,
           paymentterms               TYPE string,
           incotermsclassification    TYPE string,
           incotermslocation1         TYPE string,
           invoiceisgoodsreceiptbased TYPE abap_bool,
         END OF to_supplierpurchasingorg,
         BEGIN OF to_suppliercompany,
           companycode                TYPE string,
           paymentterms               TYPE string,
           reconciliationaccount      TYPE string,
           layoutsortingrule          TYPE string,
           istobecheckedforduplicates TYPE abap_bool,
         END OF to_suppliercompany,

         BEGIN OF to_supplier,
           to_supplierpurchasingorg TYPE TABLE OF to_supplierpurchasingorg WITH EMPTY KEY,
           to_suppliercompany       TYPE TABLE OF to_suppliercompany WITH EMPTY KEY,
         END OF to_supplier,

         BEGIN OF ty_data,
           businesspartner           TYPE string,
           businesspartnergrouping   TYPE string,
           businesspartnercategory   TYPE string,
           organizationbpname1       TYPE string,
           searchterm1               TYPE string,
           formofaddress             TYPE string,
           to_businesspartneraddress TYPE TABLE OF ty_businesspartneraddres WITH EMPTY KEY,
           to_businesspartnertax     TYPE TABLE OF ty_businesspartnertax WITH EMPTY KEY,
           to_businesspartnerrole    TYPE TABLE OF ty_businesspartnerrole WITH EMPTY KEY,
           to_businesspartnerbank    TYPE TABLE OF ty_businesspartnerbank WITH EMPTY KEY,
           to_supplier               TYPE to_supplier,
         END OF ty_data.

  DATA:lv_json TYPE string.
  DATA:ls_data TYPE ty_data.
  DATA:lv_flag TYPE abap_boolean.
  DATA:ls_businesspartneraddres TYPE ty_businesspartneraddres.
  DATA:lt_mapping TYPE /ui2/cl_json=>name_mappings.

  DATA(ls_req) = i_req-req.
  DATA(lo_dest) = zzcl_comm_tool=>get_dest( ).

  gv_businesspartner = |{  ls_req-businesspartner ALPHA = IN }|.

  IF ls_req-businesspartnergrouping NE 'Z002' AND ls_req-businesspartnergrouping NE 'Z003' AND ls_req-businesspartnergrouping NE 'Z004'.
    o_resp-msgty = 'E'.
    o_resp-msgtx = |【分组】字段值不正确，请检查| .
    RETURN.
  ENDIF.
  IF ls_req-organizationbpname1 IS INITIAL.
    o_resp-msgty = 'E'.
    o_resp-msgtx = |【供应商名称】字段不能为空| .
    RETURN.
  ENDIF.
  IF ls_req-searchterm1 IS INITIAL.
    o_resp-msgty = 'E'.
    o_resp-msgtx = |【供应商简称】字段不能为空| .
    RETURN.
  ELSE.
    IF strlen( ls_req-searchterm1 ) > 20.
      ls_req-searchterm1 = ls_req-searchterm1+0(20).
    ENDIF.
  ENDIF.
  IF ls_req-bptaxlongnumber IS INITIAL.
    o_resp-msgty = 'E'.
    o_resp-msgtx = |【纳税人识别号】字段不能为空| .
    RETURN.
  ENDIF.
  IF ls_req-currency IS INITIAL.
    o_resp-msgty = 'E'.
    o_resp-msgtx = |【采购订单货币】字段不能为空| .
    RETURN.
  ENDIF.
  IF ls_req-reconciliationaccount IS INITIAL.
    o_resp-msgty = 'E'.
    o_resp-msgtx = |【供应商统驭科目】字段不能为空| .
    RETURN.
  ENDIF.
*  IF ls_req-bankaccount IS INITIAL.
*    o_resp-msgty = 'E'.
*    o_resp-msgtx = |【供应商银行账号】字段不能为空| .
*    RETURN.
*  ENDIF.
*  IF ls_req-bankname IS INITIAL.
*    o_resp-msgty = 'E'.
*    o_resp-msgtx = |【供应商银行名称】字段不能为空| .
*    RETURN.
*  ENDIF.
  "判断创建还是修改
  SELECT COUNT(*)
    FROM i_businesspartner WITH PRIVILEGED ACCESS
   WHERE businesspartner = @gv_businesspartner.
  IF sy-subrc = 0.
    o_resp-msgty = 'E'.
    o_resp-msgtx = |供应商编码{ gv_businesspartner }已存在| .
    RETURN.
  ELSE.
    lv_flag = 'I'.
  ENDIF.

*&---导入结构JSON MAPPING
  lt_mapping = VALUE #(
       ( abap = 'BusinessPartnerGrouping'              json = 'BusinessPartnerGrouping'      )
       ( abap = 'BusinessPartnerCategory'              json = 'BusinessPartnerCategory'      )
       ( abap = 'OrganizationBPName1'                  json = 'OrganizationBPName1'          )
       ( abap = 'SearchTerm1'                          json = 'SearchTerm1'                  )
       ( abap = 'FormOfAddress'                        json = 'FormOfAddress'                )

       ( abap = 'to_BusinessPartnerAddress'            json = 'to_BusinessPartnerAddress'    )
       ( abap = 'Country'                              json = 'Country'                      )
       ( abap = 'Language'                             json = 'Language'                     )
       ( abap = 'StreetName'                           json = 'StreetName'                   )
       ( abap = 'to_PhoneNumber'                       json = 'to_PhoneNumber'               )
       ( abap = 'PhoneNumber'                          json = 'PhoneNumber'                  )
       ( abap = 'to_EmailAddress'                      json = 'to_EmailAddress'              )
       ( abap = 'EmailAddress'                         json = 'EmailAddress'                 )

       ( abap = 'to_BusinessPartnerTax'                json = 'to_BusinessPartnerTax'        )
       ( abap = 'BPTaxType'                            json = 'BPTaxType'                    )
       ( abap = 'BPTaxLongNumber'                      json = 'BPTaxLongNumber'              )

       ( abap = 'to_BusinessPartnerRole'               json = 'to_BusinessPartnerRole'       )
       ( abap = 'BusinessPartnerRole'                  json = 'BusinessPartnerRole'          )

       ( abap = 'to_supplier'                          json = 'to_Supplier'                  )
       ( abap = 'to_supplierpurchasingorg'             json = 'to_SupplierPurchasingOrg'     )
*       ( abap = 'to_suppliercompany'                   json = 'to_SupplierCompany'           )
       ( abap = 'purchasingorganization'               json = 'PurchasingOrganization'       )
       ( abap = 'currency'                             json = 'PurchaseOrderCurrency'       )
       ( abap = 'incotermsclassification'              json = 'IncotermsClassification'       )
       ( abap = 'incotermslocation1'                   json = 'IncotermsLocation1'           )
       ( abap = 'InvoiceIsGoodsReceiptBased'           json = 'InvoiceIsGoodsReceiptBased'           )

       ( abap = 'to_SalesAreaTax'                      json = 'to_SalesAreaTax'              )
       ( abap = 'DepartureCountry'                     json = 'DepartureCountry'             )
       ( abap = 'CustomerTaxCategory'                  json = 'CustomerTaxCategory'          )
       ( abap = 'CustomerTaxClassification'            json = 'CustomerTaxClassification'    )

       ( abap = 'to_PartnerFunction'                   json = 'to_PartnerFunction'           )
       ( abap = 'PartnerFunction'                      json = 'PartnerFunction'              )
       ( abap = 'PartnerCounter'                       json = 'PartnerCounter'               )

       ( abap = 'to_supplierCompany'                   json = 'to_SupplierCompany'           )
       ( abap = 'CompanyCode'                          json = 'CompanyCode'                  )
       ( abap = 'PaymentTerms'                         json = 'PaymentTerms'                 )
       ( abap = 'ReconciliationAccount'                json = 'ReconciliationAccount'        )
       ( abap = 'LayoutSortingRule'                    json = 'LayoutSortingRule'            )
       ( abap = 'IsToBeCheckedForDuplicates'           json = 'IsToBeCheckedForDuplicates'   )

       ( abap = 'to_BusinessPartnerBank'               json = 'to_BusinessPartnerBank'       )
       ( abap = 'BankIdentification'                   json = 'BankIdentification'           )
       ( abap = 'BankCountryKey'                       json = 'BankCountryKey'               )
       ( abap = 'BankNumber'                           json = 'BankNumber'                   )
       ( abap = 'bankaccountholdername'                json = 'BankAccountHolderName'        )
       ( abap = 'BankAccount'                          json = 'BankAccount'                  )
       ( abap = 'BankAccountName'                      json = 'BankAccountName'              )
       ( abap = 'BankAccountReferenceText'             json = 'BankAccountReferenceText'     )
).

  CASE ls_req-businesspartnergrouping.
    WHEN 'Z003' OR 'Z004'.
      lt_mapping = VALUE #( BASE lt_mapping
       ( abap = 'businesspartner'              json = 'BusinessPartner'      ) ).
  ENDCASE.
  CASE  lv_flag.
    WHEN 'I'.

      "默认值------BEGIN-------
      "基础视图
      ls_data-businesspartner = ls_req-businesspartner. "合作伙伴编码
      ls_data-businesspartnercategory = '2'. "业务伙伴类别
      ls_data-formofaddress = '0003'. "称谓
      ls_data-businesspartnergrouping = ls_req-businesspartnergrouping. "业务伙伴分组
      ls_data-organizationbpname1 = ls_req-organizationbpname1. "组织名称 1
      ls_data-searchterm1 = ls_req-searchterm1. "搜索项 1

      "地址
      ls_businesspartneraddres-country = 'CN'.
      ls_businesspartneraddres-language = 'ZH'.
      ls_businesspartneraddres-streetname = ls_req-streetname.
      APPEND ls_businesspartneraddres TO ls_data-to_businesspartneraddress.
      "邮箱
      IF ls_req-emailaddress IS NOT INITIAL.
        APPEND VALUE #(
         emailaddress = ls_req-emailaddress
        ) TO ls_businesspartneraddres-to_emailaddress.
      ENDIF.
      "电话
      APPEND VALUE #(
                      phonenumber = ls_req-phonenumber
      ) TO ls_businesspartneraddres-to_phonenumber.

      "税号
      IF ls_req-bptaxlongnumber IS NOT INITIAL.
        APPEND VALUE #(
                  bptaxtype       = 'CN5'
                  bptaxlongnumber = ls_req-bptaxlongnumber
         ) TO ls_data-to_businesspartnertax.
      ENDIF.

      "角色
      APPEND VALUE #(
              businesspartnerrole       = 'FLVN00'
        ) TO ls_data-to_businesspartnerrole.
      APPEND VALUE #(
              businesspartnerrole       = 'FLVN01'
        ) TO ls_data-to_businesspartnerrole.

      "银行视图
      IF ls_req-bankaccount IS NOT INITIAL.
        IF strlen( ls_req-bankaccount ) > 18.
          ls_req-bankaccountreferencetext = ls_req-bankaccount+18.
          ls_req-bankaccount = ls_req-bankaccount+0(18).
        ENDIF.
        APPEND VALUE #(
                    bankidentification = '0001'      "标识
                    bankcountrykey = 'CN'            "银行国家
                    banknumber = '9999999999'   "银行代码
                    bankaccount = ls_req-bankaccount "银行账号
                    bankaccountname = ls_req-bankaccountname "银联账号
                    bankaccountholdername = ls_req-bankname "银行名称
                    bankaccountreferencetext = ls_req-bankaccountreferencetext "参考明细
         ) TO ls_data-to_businesspartnerbank.
      ENDIF.
      "采购-采购组织视图
      SELECT DISTINCT purchasingorganization
        FROM i_purchasingorganization
        INTO TABLE @DATA(lt_purchasingorganization).
*      LOOP AT lt_purchasingorganization ASSIGNING FIELD-SYMBOL(<fs_purchasingorganization>).
*        ls_data-to_supplier-to_supplierpurchasingorg = VALUE #( BASE ls_data-to_supplier-to_supplierpurchasingorg
*                                                                ( purchasingorganization = '1100'
*                                                                currency      = ls_req-currency
*                                                                paymentterms  = ls_req-paymentterms             "
*                                                                incotermsclassification = 'DAP'
*                                                                incotermslocation1 = ls_req-organizationbpname1
*                                                                invoiceisgoodsreceiptbased = 'X' ) ).
*      ENDLOOP.
      ls_data-to_supplier-to_supplierpurchasingorg = VALUE #( ( purchasingorganization = '1100'
                                                                currency      = ls_req-currency
                                                                paymentterms  = ls_req-paymentterms             "
                                                                incotermsclassification = 'DAP'
                                                                incotermslocation1 = ls_req-organizationbpname1
                                                                invoiceisgoodsreceiptbased = 'X' )
                                                              ( purchasingorganization = '1200'
                                                                currency      = ls_req-currency
                                                                paymentterms  = ls_req-paymentterms             "
                                                                incotermsclassification = 'DAP'
                                                                incotermslocation1 = ls_req-organizationbpname1
                                                                invoiceisgoodsreceiptbased = 'X' )
                                                              ( purchasingorganization = '3100'
                                                                currency      = ls_req-currency
                                                                paymentterms  = ls_req-paymentterms             "
                                                                incotermsclassification = 'DAP'
                                                                incotermslocation1 = ls_req-organizationbpname1
                                                                invoiceisgoodsreceiptbased = 'X' )
                                                              ( purchasingorganization = '4100'
                                                                currency      = ls_req-currency
                                                                paymentterms  = ls_req-paymentterms             "
                                                                incotermsclassification = 'DAP'
                                                                incotermslocation1 = ls_req-organizationbpname1
                                                                invoiceisgoodsreceiptbased = 'X' )
                                                              ( purchasingorganization = '3260'
                                                                currency      = ls_req-currency
                                                                paymentterms  = ls_req-paymentterms             "
                                                                incotermsclassification = 'DAP'
                                                                incotermslocation1 = ls_req-organizationbpname1
                                                                invoiceisgoodsreceiptbased = 'X' )
                                                              ( purchasingorganization = '3270'
                                                                currency      = ls_req-currency
                                                                paymentterms  = ls_req-paymentterms             "
                                                                incotermsclassification = 'DAP'
                                                                incotermslocation1 = ls_req-organizationbpname1
                                                                invoiceisgoodsreceiptbased = 'X' )
                                                              ( purchasingorganization = '6001'
                                                                currency      = ls_req-currency
                                                                paymentterms  = ls_req-paymentterms             "
                                                                incotermsclassification = 'DAP'
                                                                incotermslocation1 = ls_req-organizationbpname1
                                                                invoiceisgoodsreceiptbased = 'X' ) ).



      "采购-公司代码视图
      SELECT DISTINCT companycode
        FROM i_companycode
        INTO TABLE @DATA(lt_companycode).
*      LOOP AT lt_companycode ASSIGNING FIELD-SYMBOL(<fs_companycode>).
*        ls_data-to_supplier-to_suppliercompany = VALUE #(  BASE ls_data-to_supplier-to_suppliercompany
*                                                           (  companycode            = <fs_companycode>-companycode           "公司代码
*                                                           paymentterms           = ls_req-paymentterms           "付款条件
*                                                           reconciliationaccount  = ls_req-reconciliationaccount     "统驭科目
*                                                           layoutsortingrule      = '009'
*                                                           istobecheckedforduplicates = 'X' ) ).
*      ENDLOOP.
      ls_data-to_supplier-to_suppliercompany = VALUE #( (  companycode            = '1100'           "公司代码
                                                           paymentterms           = ls_req-paymentterms           "付款条件
                                                           reconciliationaccount  = ls_req-reconciliationaccount     "统驭科目
                                                           layoutsortingrule      = '009'
                                                           istobecheckedforduplicates = 'X' )
                                                        (  companycode            = '1200'           "公司代码
                                                           paymentterms           = ls_req-paymentterms           "付款条件
                                                           reconciliationaccount  = ls_req-reconciliationaccount     "统驭科目
                                                           layoutsortingrule      = '009'
                                                           istobecheckedforduplicates = 'X' )
                                                        (  companycode            = '3100'           "公司代码
                                                           paymentterms           = ls_req-paymentterms           "付款条件
                                                           reconciliationaccount  = ls_req-reconciliationaccount     "统驭科目
                                                           layoutsortingrule      = '009'
                                                           istobecheckedforduplicates = 'X' )
                                                        (  companycode            = '4100'           "公司代码
                                                           paymentterms           = ls_req-paymentterms           "付款条件
                                                           reconciliationaccount  = ls_req-reconciliationaccount     "统驭科目
                                                           layoutsortingrule      = '009'
                                                           istobecheckedforduplicates = 'X' )
                                                        (  companycode            = '3260'           "公司代码
                                                           paymentterms           = ls_req-paymentterms           "付款条件
                                                           reconciliationaccount  = ls_req-reconciliationaccount     "统驭科目
                                                           layoutsortingrule      = '009'
                                                           istobecheckedforduplicates = 'X' )
                                                        (  companycode            = '3270'           "公司代码
                                                           paymentterms           = ls_req-paymentterms           "付款条件
                                                           reconciliationaccount  = ls_req-reconciliationaccount     "统驭科目
                                                           layoutsortingrule      = '009'
                                                           istobecheckedforduplicates = 'X' )
                                                        (  companycode            = '6001'           "公司代码
                                                           paymentterms           = ls_req-paymentterms           "付款条件
                                                           reconciliationaccount  = ls_req-reconciliationaccount     "统驭科目
                                                           layoutsortingrule      = '009'
                                                           istobecheckedforduplicates = 'X' )
                                                        (  companycode            = '3110'           "公司代码
                                                           paymentterms           = ls_req-paymentterms           "付款条件
                                                           reconciliationaccount  = ls_req-reconciliationaccount     "统驭科目
                                                           layoutsortingrule      = '009'
                                                           istobecheckedforduplicates = 'X' )
                                                        (  companycode            = '3120'           "公司代码
                                                           paymentterms           = ls_req-paymentterms           "付款条件
                                                           reconciliationaccount  = ls_req-reconciliationaccount     "统驭科目
                                                           layoutsortingrule      = '009'
                                                           istobecheckedforduplicates = 'X' )
                                                        (  companycode            = '3130'           "公司代码
                                                           paymentterms           = ls_req-paymentterms           "付款条件
                                                           reconciliationaccount  = ls_req-reconciliationaccount     "统驭科目
                                                           layoutsortingrule      = '009'
                                                           istobecheckedforduplicates = 'X' )
                                                        (  companycode            = '3140'           "公司代码
                                                           paymentterms           = ls_req-paymentterms           "付款条件
                                                           reconciliationaccount  = ls_req-reconciliationaccount     "统驭科目
                                                           layoutsortingrule      = '009'
                                                           istobecheckedforduplicates = 'X' )
                                                        (  companycode            = '3150'           "公司代码
                                                           paymentterms           = ls_req-paymentterms           "付款条件
                                                           reconciliationaccount  = ls_req-reconciliationaccount     "统驭科目
                                                           layoutsortingrule      = '009'
                                                           istobecheckedforduplicates = 'X' )
                                                        (  companycode            = '3200'           "公司代码
                                                           paymentterms           = ls_req-paymentterms           "付款条件
                                                           reconciliationaccount  = ls_req-reconciliationaccount     "统驭科目
                                                           layoutsortingrule      = '009'
                                                           istobecheckedforduplicates = 'X' )
                                                        (  companycode            = '3210'           "公司代码
                                                           paymentterms           = ls_req-paymentterms           "付款条件
                                                           reconciliationaccount  = ls_req-reconciliationaccount     "统驭科目
                                                           layoutsortingrule      = '009'
                                                           istobecheckedforduplicates = 'X' )
                                                        (  companycode            = '3220'           "公司代码
                                                           paymentterms           = ls_req-paymentterms           "付款条件
                                                           reconciliationaccount  = ls_req-reconciliationaccount     "统驭科目
                                                           layoutsortingrule      = '009'
                                                           istobecheckedforduplicates = 'X' )
                                                        (  companycode            = '3230'           "公司代码
                                                           paymentterms           = ls_req-paymentterms           "付款条件
                                                           reconciliationaccount  = ls_req-reconciliationaccount     "统驭科目
                                                           layoutsortingrule      = '009'
                                                           istobecheckedforduplicates = 'X' )
                                                        (  companycode            = '3240'           "公司代码
                                                           paymentterms           = ls_req-paymentterms           "付款条件
                                                           reconciliationaccount  = ls_req-reconciliationaccount     "统驭科目
                                                           layoutsortingrule      = '009'
                                                           istobecheckedforduplicates = 'X' )
                                                        (  companycode            = '3250'           "公司代码
                                                           paymentterms           = ls_req-paymentterms           "付款条件
                                                           reconciliationaccount  = ls_req-reconciliationaccount     "统驭科目
                                                           layoutsortingrule      = '009'
                                                           istobecheckedforduplicates = 'X' ) ).
      "默认值------END-------


*&---接口HTTP 链接调用
      TRY.
          DATA(lo_http_client) = cl_web_http_client_manager=>create_by_http_destination( lo_dest ).
          DATA(lo_request) = lo_http_client->get_http_request(   ).
          lo_http_client->enable_path_prefix( ).

          DATA(lv_uri_path) = |/API_BUSINESS_PARTNER/A_BusinessPartner|.

          lv_uri_path = lv_uri_path && |?sap-language=zh|.
          lo_request->set_uri_path( EXPORTING i_uri_path = lv_uri_path ).
          lo_request->set_header_field( i_name = 'Accept' i_value = 'application/json' ).
          "lo_request->set_header_field( i_name = 'If-Match' i_value = '*' ).
          lo_http_client->set_csrf_token(  ).

          lo_request->set_content_type( 'application/json' ).
          "传入数据转JSON
          lv_json = /ui2/cl_json=>serialize(
                data          = ls_data
                compress      = abap_true
                name_mappings = lt_mapping ).

          lo_request->set_text( lv_json ).

*&---执行http post 方法
          DATA(lo_response) = lo_http_client->execute( if_web_http_client=>post ).
*&---获取http reponse 数据
          DATA(lv_res) = lo_response->get_text(  ).
*&---确定http 状态
          DATA(status) = lo_response->get_status( ).
          IF status-code = '201'.
            TYPES:BEGIN OF ty_heads,
                    businesspartner TYPE string,
                  END OF ty_heads,
                  BEGIN OF ty_ress,
                    d TYPE ty_heads,
                  END OF  ty_ress.
            DATA:ls_ress TYPE ty_ress.
            /ui2/cl_json=>deserialize( EXPORTING json  = lv_res
                                        CHANGING data  = ls_ress ).

            o_resp-msgty  = 'S'.
            o_resp-msgtx  = '处理成功'.
            o_resp-sapnum = |{ ls_ress-d-businesspartner }|.

          ELSE.
            DATA:ls_rese TYPE zzs_odata_fail.
            /ui2/cl_json=>deserialize( EXPORTING json  = lv_res
                                        CHANGING data  = ls_rese ).
            o_resp-msgty = 'E'.
            o_resp-msgtx = ls_rese-error-message-value .

          ENDIF.

        CATCH cx_web_http_client_error INTO DATA(lx_web_http_client_error).
          o_resp-msgty = 'E'.
          o_resp-msgtx = lx_web_http_client_error->get_longtext( ) .
          RETURN.
      ENDTRY.
      "关闭HTTP链接
      IF lo_http_client IS NOT INITIAL.
        TRY.
            lo_http_client->close( ).
          CATCH cx_web_http_client_error.
            "handle exception
        ENDTRY.
      ENDIF.

  ENDCASE.

ENDFUNCTION.
