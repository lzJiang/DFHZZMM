FUNCTION zzfm_mm_004.
*"----------------------------------------------------------------------
*"*"本地接口：
*"  IMPORTING
*"     REFERENCE(I_REQ) TYPE  ZZS_MMI004_REQ OPTIONAL
*"  EXPORTING
*"     REFERENCE(O_RESP) TYPE  ZZS_REST_OUT
*"----------------------------------------------------------------------
  " You can use the template 'functionModuleParameter' to add here the signature!
  .

  TYPES:BEGIN OF ty_item,
          supplierinvoiceitem         TYPE string,
          purchaseorder               TYPE string,
          purchaseorderitem           TYPE string,
          referencedocument           TYPE string,
          referencedocumentfiscalyear TYPE string,
          referencedocumentitem       TYPE string,
          documentcurrency            TYPE string,
          supplierinvoiceitemamount   TYPE string,
          taxcode                     TYPE string,
          quantityinpurchaseorderunit TYPE string,
          purchaseorderquantityunit   TYPE string,
        END OF ty_item,
        BEGIN OF tty_item,
          results TYPE TABLE OF ty_item WITH EMPTY KEY,
        END OF tty_item,
        BEGIN OF ty_data,
          documentheadertext            TYPE bktxt,
          companycode                   TYPE string,
          documentdate                  TYPE string,
          postingdate                   TYPE string,
          supplierinvoiceidbyinvcgparty TYPE string,
          invoicingparty                TYPE string,
          documentcurrency              TYPE string,
          invoicegrossamount            TYPE string,
          duecalculationbasedate        TYPE string,
          taxiscalculatedautomatically  TYPE abap_bool,
          taxdeterminationdate          TYPE string,
          supplierinvoicestatus         TYPE string,
          to_suplrinvcitempurordref     TYPE tty_item,
        END OF ty_data.

  DATA:lv_date        TYPE string.
  DATA:lv_json TYPE string.
  DATA:lt_mapping TYPE /ui2/cl_json=>name_mappings.

  DATA:ls_data     TYPE ty_data,
       ls_results  TYPE ty_item,
       lv_material TYPE matnr,
       lv_count    TYPE i.

  DATA(ls_req) = i_req-req.
*&---=============================使用API 步骤01
  DATA(lo_dest) = zzcl_comm_tool=>get_dest( ).



*&---导入结构JSON MAPPING
  lt_mapping = VALUE #(
       ( abap = 'CompanyCode'                    json = 'CompanyCode'                )
       ( abap = 'DocumentDate'                   json = 'DocumentDate'               )
       ( abap = 'PostingDate'                    json = 'PostingDate'                )
       ( abap = 'SupplierInvoiceIDByInvcgParty'  json = 'SupplierInvoiceIDByInvcgParty'   )
       ( abap = 'DocumentHeaderText'             json = 'DocumentHeaderText'                )
       ( abap = 'InvoicingParty'                 json = 'InvoicingParty'                )
       ( abap = 'TaxCode'                        json = 'TaxCode'                )
       ( abap = 'InvoiceGrossAmount'             json = 'InvoiceGrossAmount'                )
       ( abap = 'DueCalculationBaseDate'         json = 'DueCalculationBaseDate'                )
       ( abap = 'TaxIsCalculatedAutomatically'   json = 'TaxIsCalculatedAutomatically'                )
       ( abap = 'TaxDeterminationDate'           json = 'TaxDeterminationDate'                )
       ( abap = 'SupplierInvoiceStatus'          json = 'SupplierInvoiceStatus'                )
       ( abap = 'to_SuplrInvcItemPurOrdRef'      json = 'to_SuplrInvcItemPurOrdRef'          )
       ( abap = 'results'                        json = 'results'                  )

       ( abap = 'SupplierInvoiceItem'            json = 'SupplierInvoiceItem'          )
       ( abap = 'PurchaseOrder'                  json = 'PurchaseOrder'          )
       ( abap = 'PurchaseOrderItem'              json = 'PurchaseOrderItem'          )
       ( abap = 'ReferenceDocument'              json = 'ReferenceDocument'          )
       ( abap = 'ReferenceDocumentFiscalYear'    json = 'ReferenceDocumentFiscalYear'        )
       ( abap = 'ReferenceDocumentItem'          json = 'ReferenceDocumentItem'        )
       ( abap = 'DocumentCurrency'               json = 'DocumentCurrency'        )
       ( abap = 'SupplierInvoiceItemAmount'      json = 'SupplierInvoiceItemAmount'        )
       ( abap = 'QuantityInPurchaseOrderUnit'    json = 'QuantityInPurchaseOrderUnit'        )
       ( abap = 'PurchaseOrderQuantityUnit'      json = 'PurchaseOrderQuantityUnit'        )
     ).

  IF ls_req-head-supplierinvoiceidbyinvcgparty IS NOT INITIAL.
    SELECT SINGLE *
             FROM i_supplierinvoiceapi01 WITH PRIVILEGED ACCESS
            WHERE supplierinvoiceidbyinvcgparty = @ls_req-head-supplierinvoiceidbyinvcgparty
             INTO @DATA(ls_supplierinvoice).
    IF sy-subrc = 0.
      o_resp-msgty = 'E'.
      o_resp-msgtx = |外围单据【{ ls_req-head-supplierinvoiceidbyinvcgparty }】已创建SAP预制发票{ ls_supplierinvoice-supplierinvoice }|
      && |-{ ls_supplierinvoice-fiscalyear }，请勿重复创建|.
      RETURN.
    ENDIF.
  ENDIF.

  "公司代码
  ls_data-companycode = ls_req-head-companycode.
  "抬头文本
  ls_data-documentheadertext = ls_req-head-documentheadertext.
  "发票日期
  ls_data-documentdate = zzcl_comm_tool=>date2iso( ls_req-head-documentdate ).
  "过账日期
  ls_data-postingdate = zzcl_comm_tool=>date2iso( ls_req-head-postingdate ).
  "参考
  ls_data-supplierinvoiceidbyinvcgparty = ls_req-head-supplierinvoiceidbyinvcgparty.
  "开发票方
  ls_data-invoicingparty = ls_req-head-invoicingparty.
  "货币
  ls_data-documentcurrency = ls_req-head-documentcurrency.
  "发票总含税金额
  ls_data-invoicegrossamount = ls_req-head-invoicegrossamount.

  data(lv_date_now) = cl_abap_context_info=>get_system_date( ).
  ls_req-head-duecalculationbasedate = lv_date_now.
  ls_data-duecalculationbasedate =  zzcl_comm_tool=>date2iso( ls_req-head-duecalculationbasedate ).

  ls_data-taxiscalculatedautomatically = ls_req-head-taxiscalculatedautomatically.
  ls_data-supplierinvoicestatus = ls_req-head-supplierinvoicestatus.
  ls_data-taxdeterminationdate = zzcl_comm_tool=>date2iso( ls_req-head-postingdate ).

  LOOP AT ls_req-item INTO DATA(ls_item).
    CLEAr:lv_material.
    zcl_com_util=>matnr_zero_in( EXPORTING input = ls_item-purchaseorderitem
                                 IMPORTING output =  lv_material ).
    SELECT SINGLE b~*
        FROM i_materialdocumentheader_2 WITH PRIVILEGED ACCESS AS a
        INNER JOIN i_materialdocumentitem_2 WITH PRIVILEGED ACCESS AS b
          ON a~materialdocument = b~materialdocument AND a~materialdocumentyear = b~materialdocumentyear
       WHERE materialdocumentheadertext = @ls_item-referencedocument
         AND b~goodsmovementiscancelled = ''
         AND b~reversedmaterialdocument = ''
         AND b~Material = @lv_material
        INTO @DATA(ls_materialdocumentitem).
    IF sy-subrc = 0.
      ls_item-referencedocument = ls_materialdocumentitem-materialdocument.
      ls_item-referencedocumentfiscalyear = ls_materialdocumentitem-materialdocumentyear.
      ls_item-referencedocumentitem = ls_materialdocumentitem-materialdocumentitem.
      ls_item-purchaseorderitem  = ls_materialdocumentitem-purchaseorderitem.
    ELSE.
      o_resp-msgty = 'E'.
      o_resp-msgtx = |未找到WMS入库单【{ ls_item-referencedocument }】产品【{ ls_item-purchaseorderitem }】对应SAP物料凭证行，请联系WMS管理员|.
      RETURN.
    ENDIF.
    CLEAR:ls_results.
    MOVE-CORRESPONDING ls_item TO ls_results.
    lv_count = lv_count + 1.
    ls_results-supplierinvoiceitem = lv_count.
    APPEND ls_results TO ls_data-to_suplrinvcitempurordref-results.
  ENDLOOP.

*&---接口HTTP 链接调用
  TRY.
      DATA(lo_http_client) = cl_web_http_client_manager=>create_by_http_destination( lo_dest ).
      DATA(lo_request) = lo_http_client->get_http_request(   ).
      lo_http_client->enable_path_prefix( ).

      DATA(lv_uri_path) = |/API_SUPPLIERINVOICE_PROCESS_SRV/A_SupplierInvoice?sap-language=zh|.
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
                supplierinvoice TYPE string,
                fiscalyear      TYPE string,
              END OF ty_heads,
              BEGIN OF ty_ress,
                d TYPE ty_heads,
              END OF  ty_ress.
        DATA:ls_ress TYPE ty_ress.
        /ui2/cl_json=>deserialize( EXPORTING json  = lv_res
                                    CHANGING data  = ls_ress ).

        o_resp-msgty  = 'S'.
        o_resp-msgtx  = 'success'.
        o_resp-sapnum = |{ ls_ress-d-supplierinvoice }-{ ls_ress-d-fiscalyear }|.
      ELSE.
        DATA:ls_rese TYPE zzs_odata_fail.
        /ui2/cl_json=>deserialize( EXPORTING json  = lv_res
                                    CHANGING data  = ls_rese ).
        o_resp-msgty = 'E'.
        o_resp-msgtx = ls_rese-error-message-value .

      ENDIF.
    CATCH cx_web_http_client_error INTO DATA(lx_web_http_client_error).
      RETURN.
  ENDTRY.

ENDFUNCTION.
