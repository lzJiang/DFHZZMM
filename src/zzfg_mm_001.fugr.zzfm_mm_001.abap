FUNCTION zzfm_mm_001.
*"----------------------------------------------------------------------
*"*"本地接口：
*"  IMPORTING
*"     REFERENCE(I_REQ) TYPE  ZZS_MMI001_REQ OPTIONAL
*"  EXPORTING
*"     REFERENCE(O_RESP) TYPE  ZZS_REST_OUT
*"----------------------------------------------------------------------
  .

  TYPES:BEGIN OF ty_item,
          goodsmovementtype            TYPE string,
          goodsmovementrefdoctype      TYPE string,
          delivery                     TYPE i_deliverydocumentitem-deliverydocument,
          deliveryitem                 TYPE string,
          purchaseorder                TYPE i_materialdocumentitem_2-purchaseorder,
          purchaseorderitem            TYPE string,
          manufacturingorder           TYPE aufnr,
          manufacturingorderitem       TYPE string,
          reservation                  TYPE string,
          reservationitem              TYPE string,
          costcenter                   TYPE string,
          material                     TYPE matnr,
          plant                        TYPE string,
          storagelocation              TYPE string,
          batch                        TYPE string,
          manufacturedate              TYPE string,
          quantityinbaseunit           TYPE string,
          entryunit                    TYPE string,
          quantityinentryunit          TYPE string,
          issgorrcvgmaterial           TYPE string,
          issuingorreceivingplant      TYPE string,
          issuingorreceivingstorageloc TYPE string,
          issgorrcvgbatch              TYPE string,
          shelflifeexpirationdate      TYPE string,
          reversedmaterialdocumentyear TYPE string,
          reversedmaterialdocument     TYPE string,
          reversedmaterialdocumentitem TYPE string,
          invtrymgmtreferencedocument  TYPE string,
          invtrymgmtrefdocumentitem    TYPE string,
          goodsmovementreasoncode      TYPE string,
          supplier                     TYPE string,
          customer                     TYPE string,
          materialdocumentitemtext     TYPE string,
          inventoryspecialstocktype    TYPE string,
          materialdocumentline         TYPE string,
          materialdocumentparentline   TYPE string,
          hierarchynodelevel           TYPE string,
          wbselement                   TYPE string,
          batchbysupplier(15)          TYPE c,
          inventorystocktype           TYPE string,
          inventoryusabilitycode       TYPE string,
        END OF ty_item,
        BEGIN OF tty_item,
          results TYPE TABLE OF ty_item WITH EMPTY KEY,
        END OF tty_item,
        BEGIN OF ty_data,
          documentdate               TYPE string,
          postingdate                TYPE string,
          referencedocument          TYPE string,
          goodsmovementcode          TYPE string,
          materialdocumentheadertext TYPE string,
          to_materialdocumentitem    TYPE tty_item,
        END OF ty_data.

  DATA:lv_date        TYPE string.
  DATA:lv_json TYPE string.
  DATA:lt_mapping TYPE /ui2/cl_json=>name_mappings.
  DATA:ls_data   TYPE ty_data,
       ls_item   TYPE ty_item,
       ls_sub    TYPE ty_item,
       ls_parent TYPE ty_item,
       lt_item   TYPE TABLE OF ty_item.
  DATA:ls_tmp TYPE zzs_mmi001_req.
  DATA:lv_mater18(18).
  DATA:lv_deliveryitem TYPE i_deliverydocumentitem-deliverydocumentitem.
*&---BAPI参数
  DATA:lv_msg  TYPE bapi_msg,
       lv_msg2 TYPE bapi_msg.
  DATA:lv_tabix TYPE i_materialdocumentitem_2-materialdocumentitem.
  DATA:lv_purchaseorderitem TYPE i_materialdocumentitem_2-purchaseorderitem,
       lv_purchaseorder_ww  TYPE i_materialdocumentitem_2-purchaseorder. "WMS委外采购订单
  DATA:lv_line_id    TYPE i_materialdocumentitem_2-materialdocumentline,
       lv_parent_id  TYPE i_materialdocumentitem_2-materialdocumentparentline,
       lv_line_depth TYPE numc2.
  DATA:lv_quantity TYPE i_posubcontractingcompapi01-requiredquantity.
  DATA:lv_remain TYPE i_posubcontractingcompapi01-requiredquantity.
  DATA:lv_purmaterial TYPE i_purchaseorderitemapi01-material.
  DATA:lv_supplier   TYPE i_materialdocumentitem_2-supplier,
       ls_zztmm_0005 TYPE zztmm_0005,
       lt_zztmm_0005 TYPE TABLE OF zztmm_0005,
       lv_543_flag   TYPE char1,
       lt_zzlast_pur TYPE zzt_mmi003_0002_in,
       ls_zzlast_pur TYPE zzs_mmi003_0002_in.
  DATA:lt_zztmm_0006 TYPE TABLE OF zztmm_0006,
       ls_zztmm_0006 TYPE zztmm_0006.
  DATA:lt_item_zt3 TYPE TABLE OF ty_item, "公司间委外订单收货-需创建交货单发货过账后再收货
       ls_item_zt3 TYPE ty_item.
  ls_tmp = i_req.
*&---=============================使用API 步骤01
  DATA(lo_dest) = zzcl_comm_tool=>get_dest( ).


*&---导入结构JSON MAPPING
  lt_mapping = VALUE #(
       ( abap = 'DocumentDate'                 json = 'DocumentDate'                )
       ( abap = 'PostingDate'                  json = 'PostingDate'                 )
       ( abap = 'ReferenceDocument'            json = 'ReferenceDocument'           )
       ( abap = 'GoodsMovementCode'            json = 'GoodsMovementCode'           )
       ( abap = 'MaterialDocumentHeaderText'   json = 'MaterialDocumentHeaderText'  )
       ( abap = 'to_MaterialDocumentItem'      json = 'to_MaterialDocumentItem'     )
       ( abap = 'results'                      json = 'results'                     )
       ( abap = 'GoodsMovementRefDocType'      json = 'GoodsMovementRefDocType'     )
       ( abap = 'GoodsMovementType'            json = 'GoodsMovementType'           )
       ( abap = 'Delivery'                     json = 'Delivery'                    )
       ( abap = 'DeliveryItem'                 json = 'DeliveryItem'                )
       ( abap = 'PurchaseOrder'                json = 'PurchaseOrder'               )
       ( abap = 'PurchaseOrderItem'            json = 'PurchaseOrderItem'           )
       ( abap = 'ManufacturingOrder'           json = 'ManufacturingOrder'          )
       ( abap = 'ManufacturingOrderItem'       json = 'ManufacturingOrderItem'      )
       ( abap = 'Reservation'                  json = 'Reservation'                 )
       ( abap = 'ReservationItem'              json = 'ReservationItem'             )
       ( abap = 'CostCenter'                   json = 'CostCenter'                  )
       ( abap = 'Material'                     json = 'Material'                    )
       ( abap = 'Plant'                        json = 'Plant'                       )
       ( abap = 'StorageLocation'              json = 'StorageLocation'             )
       ( abap = 'Batch'                        json = 'Batch'                       )
       ( abap = 'ManufactureDate'              json = 'ManufactureDate'             )
       ( abap = 'EntryUnit'                    json = 'EntryUnit'                   )
       ( abap = 'QuantityInEntryUnit'          json = 'QuantityInEntryUnit'         )
       ( abap = 'QuantityInBaseUnit'           json = 'QuantityInBaseUnit'          )
       ( abap = 'IssgOrRcvgMaterial'           json = 'IssgOrRcvgMaterial'          )
       ( abap = 'IssuingOrReceivingPlant'      json = 'IssuingOrReceivingPlant'     )
       ( abap = 'IssuingOrReceivingStorageLoc' json = 'IssuingOrReceivingStorageLoc' )
       ( abap = 'IssgOrRcvgBatch'              json = 'IssgOrRcvgBatch'             )
       ( abap = 'ShelfLifeExpirationDate'      json = 'ShelfLifeExpirationDate'     )
       ( abap = 'ReversedMaterialDocumentYear' json = 'ReversedMaterialDocumentYear' )
       ( abap = 'ReversedMaterialDocument'     json = 'ReversedMaterialDocument'    )
       ( abap = 'ReversedMaterialDocumentItem' json = 'ReversedMaterialDocumentItem' )
       ( abap = 'InvtryMgmtReferenceDocument'  json = 'InvtryMgmtReferenceDocument' )
       ( abap = 'InvtryMgmtRefDocumentItem'    json = 'InvtryMgmtRefDocumentItem'   )
       ( abap = 'GoodsMovementReasonCode'      json = 'GoodsMovementReasonCode'     )
       ( abap = 'BatchBySupplier'              json = 'BatchBySupplier'             )

       ( abap = 'Supplier'                     json = 'Supplier'                    )
       ( abap = 'Customer'                     json = 'Customer'                    )
       ( abap = 'InventorySpecialStockType'    json = 'InventorySpecialStockType'   )
       ( abap = 'InventoryStockType'           json = 'InventoryStockType'          )
       ( abap = 'InventoryUsabilityCode'       json = 'InventoryUsabilityCode'      )
       ( abap = 'MaterialDocumentItemText'     json = 'MaterialDocumentItemText'    )
       ( abap = 'WBSElement'                   json = 'WBSElement'                  )

       ( abap = 'MaterialDocumentLine'         json = 'MaterialDocumentLine'        )
       ( abap = 'MaterialDocumentParentLine'   json = 'MaterialDocumentParentLine'  )
       ( abap = 'HierarchyNodeLevel'           json = 'HierarchyNodeLevel'  )
    ).

  IF ls_tmp-req-head-materialdocumentheadertext IS NOT INITIAL.
    SELECT SINGLE a~*
           FROM i_materialdocumentheader_2 WITH PRIVILEGED ACCESS AS a
           INNER JOIN i_materialdocumentitem_2 WITH PRIVILEGED ACCESS AS b
             ON a~materialdocument = b~materialdocument AND a~materialdocumentyear = b~materialdocumentyear
          WHERE materialdocumentheadertext = @ls_tmp-req-head-materialdocumentheadertext
            AND goodsmovementiscancelled = ''
            AND b~reversedmaterialdocument = ''
           INTO @DATA(ls_materialdocumentheader).
    IF sy-subrc = 0.
      o_resp-msgty = 'S'.
      o_resp-msgtx = |外围单据【{ ls_tmp-req-head-materialdocumentheadertext }】已生成SAP物料凭证{ ls_materialdocumentheader-materialdocument  }|
      && |-{ ls_materialdocumentheader-materialdocumentyear },请勿重复推送| .
      o_resp-sapnum = |{ ls_materialdocumentheader-materialdocument }-{ ls_materialdocumentheader-materialdocumentyear }| .
      RETURN.
    ENDIF.
  ENDIF.

  DATA(lv_user) = cl_abap_context_info=>get_user_technical_name( ).

  "数据整合
  "凭证日期
  ls_data-documentdate = zzcl_comm_tool=>date2iso( ls_tmp-req-head-documentdate ).
  "过账日期
  ls_data-postingdate = zzcl_comm_tool=>date2iso( ls_tmp-req-head-postingdate ).
  "库存业务类型
  ls_data-goodsmovementcode = ls_tmp-req-head-goodsmovementcode.
  "抬头文本
  ls_data-materialdocumentheadertext = ls_tmp-req-head-materialdocumentheadertext.
  LOOP AT ls_tmp-req-item INTO DATA(ls_tmp_item).
    CLEAR:ls_item,lv_purchaseorderitem,lv_purmaterial,lv_purchaseorder_ww.
    lv_tabix = lv_tabix + 1.
    lv_line_id = lv_line_id + 1.
    ls_tmp_item-batch = to_upper( ls_tmp_item-batch ).
    ls_tmp_item-zzwmsbatch = to_upper( ls_tmp_item-batch ).
    ls_tmp_item-issgorrcvgbatch = to_upper( ls_tmp_item-issgorrcvgbatch ).
    MOVE-CORRESPONDING ls_tmp_item TO ls_item.

    ls_item-manufacturingorder = |{ ls_item-manufacturingorder ALPHA = IN }|.
    ls_item-purchaseorder = |{ ls_item-purchaseorder ALPHA = IN }|.
    ls_item-delivery = |{ ls_item-delivery ALPHA = IN }|.
    lv_mater18 = ls_tmp_item-material.
    lv_mater18 = |{ lv_mater18 ALPHA = IN }|.
    ls_tmp_item-material = lv_mater18.
    lv_deliveryitem = ls_item-deliveryitem.

    "供应商批次
    IF ls_tmp_item-batch IS NOT INITIAL.
      ls_item-batchbysupplier = ls_tmp_item-batch.
    ENDIF.
    IF ls_tmp_item-zzwmsbatch IS NOT INITIAL.
      ls_item-batchbysupplier = ls_tmp_item-zzwmsbatch.
    ENDIF.
    "匹配批次
    IF ls_item-batch IS NOT INITIAL AND lv_user = 'CC0000000002'.
      SELECT SINGLE a~material,
                a~batch
        FROM i_batchcharacteristicvaluetp_2 WITH PRIVILEGED ACCESS AS a
        JOIN i_clfncharacteristic WITH PRIVILEGED ACCESS AS b ON a~charcinternalid = b~charcinternalid
       WHERE material = @lv_mater18
         AND b~characteristic = 'Z_WMSBATCH'
         AND a~charcvalue = @ls_item-batch
        INTO @DATA(ls_valuetp).
      IF sy-subrc = 0.
        ls_item-batch = ls_valuetp-batch.
        ls_tmp_item-batch = ls_item-batch.
      ELSE.
        IF ls_item-goodsmovementtype NE '101' AND ls_item-goodsmovementtype NE '501' AND lv_user = 'CC0000000002'.
          o_resp-msgty  = 'E'.
          o_resp-msgtx  = |非入库物料移动类型{ ls_item-goodsmovementtype }物料{ ls_tmp_item-material }WMS批次{ ls_item-batch }未找到对应SAP批次|.
          EXIT.
        ENDIF.
        IF ls_item-goodsmovementtype = '501'.
          ls_tmp_item-zzwmsbatch = ls_tmp_item-batch.
          CLEAR:ls_tmp_item-batch,ls_item-batch.
        ENDIF.
      ENDIF.
    ENDIF.

    IF ls_item-issgorrcvgbatch IS NOT INITIAL AND lv_user = 'CC0000000002'.
      SELECT SINGLE a~material,
                a~batch
        FROM i_batchcharacteristicvaluetp_2 WITH PRIVILEGED ACCESS AS a
        JOIN i_clfncharacteristic WITH PRIVILEGED ACCESS AS b ON a~charcinternalid = b~charcinternalid
       WHERE material = @lv_mater18
         AND b~characteristic = 'Z_WMSBATCH'
         AND a~charcvalue = @ls_item-issgorrcvgbatch
        INTO @ls_valuetp.
      IF sy-subrc = 0.
        ls_item-issgorrcvgbatch = ls_valuetp-batch.
        ls_tmp_item-issgorrcvgbatch = ls_item-issgorrcvgbatch.
      ELSE.
        IF ls_item-goodsmovementtype NE '101' AND ls_item-goodsmovementtype NE '501' AND lv_user = 'CC0000000002'.
          o_resp-msgty  = 'E'.
          o_resp-msgtx  = |非入库物料移动类型{ ls_item-goodsmovementtype }物料{ ls_tmp_item-material }WMS批次{ ls_item-issgorrcvgbatch }未找到对应SAP批次|.
          EXIT.
        ENDIF.
      ENDIF.
    ENDIF.

    "移动类型确认 code
    CASE ls_item-goodsmovementtype.
      WHEN '101' OR '102' OR  '161' OR '121' OR '543' OR '544'."采购入库 生产入库 采购退货 委外后续调整
        IF ls_item-delivery IS NOT INITIAL.
          ls_data-goodsmovementcode = '01'.
          ls_item-goodsmovementrefdoctype = 'B'.

          "行项目
          SELECT SINGLE
                 a~purchaseorder,
                 a~purchaseorderitem,
                 a~sddocumentcategory,
                 a~actualdeliveryquantity
            FROM i_deliverydocumentitem WITH PRIVILEGED ACCESS AS a
           WHERE deliverydocument = @ls_item-delivery
             AND deliverydocumentitem = @lv_deliveryitem
            INTO @DATA(ls_deliverydocumentitem).

          "汇总已交货行。
          SELECT SINGLE SUM( CASE a~debitcreditcode
                             WHEN 'S' THEN  a~quantityinentryunit
                             ELSE 0 - a~quantityinentryunit END )  AS menge
            FROM i_materialdocumentitem_2 WITH PRIVILEGED ACCESS AS a
           WHERE a~deliverydocument = @ls_item-delivery
             AND a~deliverydocumentitem = @lv_deliveryitem
             AND a~goodsmovementtype IN ('101','102')
            INTO @DATA(lv_menge).

          IF ls_item-quantityinentryunit + lv_menge > ls_deliverydocumentitem-actualdeliveryquantity.
            o_resp-msgty  = 'E'.
            o_resp-msgtx  = '收货数量不允许大于交货单数量'.
            EXIT.
          ENDIF.

          IF ls_deliverydocumentitem-sddocumentcategory = '7'.
            IF ls_tmp_item-batch IS NOT INITIAL OR ls_tmp_item-shelflifeexpirationdate IS NOT INITIAL.
              CALL FUNCTION 'ZZFM_MM_001_INB'
                EXPORTING
                  i_data = ls_tmp_item
                IMPORTING
                  o_resp = o_resp.
              IF o_resp-msgty   = 'E'.
                EXIT.
              ENDIF.
            ENDIF.
          ENDIF.

          "采购订单写入
          IF ls_item-purchaseorder IS INITIAL.
            ls_item-purchaseorder = ls_deliverydocumentitem-purchaseorder.
            ls_item-purchaseorderitem = ls_deliverydocumentitem-purchaseorderitem.
          ENDIF.
        ENDIF.

        IF ls_item-manufacturingorder IS NOT INITIAL.
          ls_data-goodsmovementcode = '02'.
          ls_item-goodsmovementrefdoctype = 'F'.
          SELECT SINGLE *
                 FROM i_manufacturingorderitem WITH PRIVILEGED ACCESS
                WHERE manufacturingorder = @ls_item-manufacturingorder
                 INTO @DATA(ls_manufacturingorderitem).
          IF sy-subrc = 0.
            ls_item-inventoryusabilitycode = ls_manufacturingorderitem-inventoryusabilitycode.
            ls_item-plant = ls_manufacturingorderitem-productionplant.
            CLEAR:ls_item-manufacturingorderitem.
          ELSE.
            lv_purchaseorder_ww = ls_tmp_item-manufacturingorder.
            lv_purchaseorder_ww = |{ lv_purchaseorder_ww ALPHA = IN }|.
            lv_purmaterial = ls_item-manufacturingorderitem.
            zcl_com_util=>matnr_zero_in( EXPORTING input = lv_purmaterial
                                         IMPORTING output = lv_purmaterial ).
            SELECT SINGLE *
                   FROM i_purchaseorderitemapi01 WITH PRIVILEGED ACCESS
                  WHERE purchaseorder = @lv_purchaseorder_ww
                    AND material = @lv_purmaterial
                   INTO @DATA(ls_purchaseorderitemapi01).
            IF sy-subrc = 0.
              ls_item-purchaseorder = lv_purchaseorder_ww.
              ls_item-purchaseorderitem = ls_purchaseorderitemapi01-purchaseorderitem.
              ls_item-goodsmovementrefdoctype = 'B'.
              CLEAR:ls_item-manufacturingorderitem,ls_item-manufacturingorder.
              SELECT SINGLE *
                   FROM i_purchaseorderapi01 WITH PRIVILEGED ACCESS
                  WHERE purchaseorder = @lv_purchaseorder_ww
                    AND purchaseordertype = 'ZT3'
                   INTO @DATA(ls_purchaseorder_zt3).
              APPEND ls_item TO lt_item_zt3.
            ELSE.
              o_resp-msgty  = 'E'.
              o_resp-msgtx  = |WMS传入订单{ ls_tmp_item-manufacturingorder }物料{ ls_tmp_item-manufacturingorderitem }未找到对应SAP自制订单或委外订单|.
              EXIT.
            ENDIF.
          ENDIF.

*          IF strlen( ls_item-purchaseorderitem ) > 5.
*
*          ELSE.
*            lv_purchaseorderitem = ls_item-purchaseorderitem.
*          ENDIF.

*          SELECT SINGLE *
*                 FROM i_purchaseorderitemapi01 WITH PRIVILEGED ACCESS
*                WHERE purchaseorder = @ls_item-purchaseorder
*                  AND purchaseorderitem = @lv_purchaseorderitem
*                 INTO @DATA(ls_purchaseorderitemapi01).
*          IF sy-subrc NE 0.
*            "委外订单收货WMS行项目传入物料号，根据物料号查找SAP行号
*            lv_purmaterial = ls_item-purchaseorderitem.
*            zcl_com_util=>matnr_zero_in( EXPORTING input = lv_purmaterial
*                                         IMPORTING output = lv_purmaterial ).
*            SELECT SINGLE *
*                   FROM i_purchaseorderitemapi01 WITH PRIVILEGED ACCESS
*                  WHERE purchaseorder = @ls_item-purchaseorder
*                    AND material = @lv_purmaterial
*                   INTO @ls_purchaseorderitemapi01.
*            IF sy-subrc = 0.
*              ls_item-purchaseorderitem = ls_purchaseorderitemapi01-purchaseorderitem.
*            ELSE.
*              o_resp-msgty  = 'E'.
*              o_resp-msgtx  = |采购订单{ ls_item-purchaseorder }行/物料{ ls_item-purchaseorderitem }未找到对应SAP采购订单行|.
*              EXIT.
*            ENDIF.
*          ENDIF.
        ENDIF.

        IF ls_item-purchaseorder IS NOT INITIAL.
          ls_data-goodsmovementcode = '01'.
          ls_item-goodsmovementrefdoctype = 'B'.

          SELECT SINGLE *
            FROM i_purchaseorderitemapi01 WITH PRIVILEGED ACCESS
           WHERE purchaseorder = @ls_item-purchaseorder
             AND purchaseorderitem = @ls_item-purchaseorderitem
             AND purchaseorderitemcategory = '3'
            INTO @DATA(ls_materialdocumentitem_1).
          IF sy-subrc = 0 AND ls_item-goodsmovementtype = '101'.
*            "委外订单收货，设置最后收获标识为X,将委外订单组件全部消耗
*            ls_tmp_item-zzlast = 'X'.
          ENDIF.
        ENDIF.

        "WMS批次特性
        IF ls_item-goodsmovementtype = '101'.
          IF ls_tmp_item-manufacturingorder IS INITIAL AND ls_tmp_item-purchaseorder IS INITIAL.
            o_resp-msgty  = 'E'.
            o_resp-msgtx  = |移动类型101行{ lv_tabix }传入自制生产订单和采购订单不能均为空|.
            EXIT.
          ENDIF.
          IF ls_tmp_item-zzwmsbatch IS NOT INITIAL AND ls_item-purchaseorder IS NOT INITIAL.
            SELECT SINGLE a~material,
                          a~batch
              FROM i_batchcharacteristicvaluetp_2 WITH PRIVILEGED ACCESS AS a
              JOIN i_clfncharacteristic WITH PRIVILEGED ACCESS AS b ON a~charcinternalid = b~charcinternalid
             WHERE material = @lv_mater18
               AND b~characteristic = 'Z_WMSBATCH'
               AND a~charcvalue = @ls_tmp_item-zzwmsbatch
              INTO @ls_valuetp.
            IF sy-subrc = 0.
              ls_item-batch = ls_valuetp-batch.
              ls_tmp_item-batch = ls_item-batch.
            ELSE.
              CLEAR:ls_item-batch.
              gv_wmsflag = abap_true.
              ls_item-materialdocumentitemtext = lv_tabix.
              ls_tmp_item-tabix = lv_tabix.
              IF strlen( ls_tmp_item-zzwmsbatch ) <= 10.
                ls_item-batch = ls_tmp_item-zzwmsbatch.
              ENDIF.
            ENDIF.
            ls_item-batchbysupplier = ls_tmp_item-zzwmsbatch.
          ENDIF.



          IF ls_item-purchaseorder IS NOT INITIAL AND ls_item-plant = '1100' AND ls_item-storagelocation NE '1000' AND ls_item-storagelocation IS NOT INITIAL.
            "收货库位不为1000基地仓时，则为供应商O库位相关收货
            lv_supplier = ls_item-storagelocation.
            lv_supplier = |{ lv_supplier ALPHA = IN }|.
            SELECT SINGLE *
                     FROM i_supplier WITH PRIVILEGED ACCESS
                    WHERE supplier = @lv_supplier
                     INTO @DATA(ls_supplier).
            IF sy-subrc = 0.
              ls_item-storagelocation = '1000'.
            ELSE.
              o_resp-msgty  = 'E'.
              o_resp-msgtx  = |传入库位相关供应商编码{ ls_item-storagelocation }在SAP不存在|.
              EXIT.
            ENDIF.
            MOVE-CORRESPONDING ls_tmp_item TO ls_zztmm_0005.
            MOVE-CORRESPONDING ls_tmp-req-head TO ls_zztmm_0005.
            ls_item-materialdocumentitemtext = lv_tabix.
            ls_tmp_item-tabix = lv_tabix.
            ls_zztmm_0005-materialdocumentitemtext = ls_tmp_item-tabix.
            ls_zztmm_0005-supplier = lv_supplier.
            ls_zztmm_0005-storagelocation = '1000'.
            APPEND ls_zztmm_0005 TO lt_zztmm_0005.
          ENDIF.
        ENDIF.
        "针对委外订单
        "1.最后一次收获A11委外扣料调整，是否最后一次收货 = X 时， 计算扣减数量 = 总需要扣减数量 - 已提货数量
        "2.委外加工补领出库单 121 + 543
        "3.委外加工退回入库单 121 + 544
        IF ls_item-purchaseorder IS NOT INITIAL
        AND ( ls_item-goodsmovementtype = '121' OR ls_item-goodsmovementtype = '543' OR ls_item-goodsmovementtype = '544' ) .
          lv_purchaseorderitem = ls_item-purchaseorderitem.
          lv_parent_id = lv_line_id.
          IF ls_tmp_item-manufacturingorder IS INITIAL.
            o_resp-msgty = 'E'.
            o_resp-msgtx = |121,543,544移动类型委外采购订单号【manufacturingorder】必传| .
            EXIT.
          ENDIF.
          IF ls_tmp_item-manufacturingorderitem IS INITIAL.
            o_resp-msgty = 'E'.
            o_resp-msgtx = |121,543,544移动类型委外采购订单产品【manufacturingorderitem】必传| .
            EXIT.
          ENDIF.
          IF ls_tmp_item-material IS INITIAL AND ( ls_item-goodsmovementtype = '543' OR ls_item-goodsmovementtype = '544' ).
            o_resp-msgty = 'E'.
            o_resp-msgtx = |543,544移动类型委外采购订单产品组件【material】必传| .
            EXIT.
          ENDIF.
          IF ls_item-purchaseorder IS INITIAL.
            o_resp-msgty = 'E'.
            o_resp-msgtx = |委外订单{ ls_tmp_item-manufacturingorder }产品{ ls_tmp_item-manufacturingorderitem }在SAP不存在| .
            EXIT.
          ENDIF.
          DATA:lt_collect_543       TYPE TABLE OF i_posubcontractingcompapi01,
               ls_collect_543       TYPE i_posubcontractingcompapi01,
               lt_collect_543_batch TYPE TABLE OF i_posubcontractingcompapi01,
               ls_collect_543_batch TYPE i_posubcontractingcompapi01.
          "委外订单组件需求数量
          SELECT a~purchaseorder,
                 a~purchaseorderitem,
                 a~material,
                 a~requiredquantity,
                 a~withdrawnquantity,
                 a~batch,
                 a~plant,
                 b~supplier
            FROM i_posubcontractingcompapi01 WITH PRIVILEGED ACCESS AS a
            LEFT JOIN i_purchaseorderapi01 WITH PRIVILEGED ACCESS AS b ON a~purchaseorder = b~purchaseorder
           WHERE a~purchaseorder = @ls_item-purchaseorder
             AND a~purchaseorderitem = @lv_purchaseorderitem
            INTO TABLE @DATA(lt_posub).

          IF lt_posub[] IS NOT INITIAL.
            "委外订单组件提货数量
            SELECT a~materialdocumentyear,
                   a~materialdocument,
                   a~materialdocumentitem,
                   a~purchaseorder,
                   a~purchaseorderitem,
                   a~material,
                   a~batch,
                   a~plant,
                   a~supplier,
                   a~goodsmovementtype,
                   a~quantityinentryunit,
                   b~materialdocumentheadertext
              FROM i_materialdocumentitem_2 WITH PRIVILEGED ACCESS AS a
              INNER JOIN i_materialdocumentheader_2 WITH PRIVILEGED ACCESS AS b
                ON a~materialdocument = b~materialdocument AND a~materialdocumentyear = b~materialdocumentyear
             WHERE a~purchaseorder = @ls_item-purchaseorder
               AND a~purchaseorderitem = @lv_purchaseorderitem
               AND a~goodsmovementtype IN ( '543','544' )
              INTO TABLE @DATA(lt_materialdocumentitem).

            LOOP AT lt_materialdocumentitem INTO DATA(ls_materialdocumentitem).
              IF ls_materialdocumentitem-materialdocumentheadertext CP 'WWBL*'
              OR ls_materialdocumentitem-materialdocumentheadertext CP 'WWTH*'.
                "排除委外补领和退料的扣料
                CONTINUE.
              ENDIF.
              ls_collect_543-purchaseorder = ls_materialdocumentitem-purchaseorder.
              ls_collect_543-purchaseorderitem = ls_materialdocumentitem-purchaseorderitem.
              ls_collect_543-material = ls_materialdocumentitem-material.
              IF ls_materialdocumentitem-goodsmovementtype = '544'.
                ls_materialdocumentitem-quantityinentryunit = ls_materialdocumentitem-quantityinentryunit * -1.
              ENDIF.
              ls_collect_543-withdrawnquantity = ls_materialdocumentitem-quantityinentryunit.
              COLLECT ls_collect_543 INTO lt_collect_543.
              CLEAR:ls_collect_543.
            ENDLOOP.
            LOOP AT lt_materialdocumentitem INTO ls_materialdocumentitem.
              IF ls_materialdocumentitem-materialdocumentheadertext CP 'WWBL*'
              OR ls_materialdocumentitem-materialdocumentheadertext CP 'WWTH*'.
                "排除委外补领和退料的扣料
                CONTINUE.
              ENDIF.
              ls_collect_543_batch-purchaseorder = ls_materialdocumentitem-purchaseorder.
              ls_collect_543_batch-purchaseorderitem = ls_materialdocumentitem-purchaseorderitem.
              ls_collect_543_batch-material = ls_materialdocumentitem-material.
              ls_collect_543_batch-batch = ls_materialdocumentitem-batch.
              ls_collect_543_batch-plant = ls_materialdocumentitem-plant.
              IF ls_materialdocumentitem-goodsmovementtype = '544'.
                ls_materialdocumentitem-quantityinentryunit = ls_materialdocumentitem-quantityinentryunit * -1.
              ENDIF.
              ls_collect_543_batch-withdrawnquantity = ls_materialdocumentitem-quantityinentryunit.
              COLLECT ls_collect_543_batch INTO lt_collect_543_batch.
              CLEAR:ls_collect_543_batch.
            ENDLOOP.
            SORT lt_collect_543 BY purchaseorder purchaseorderitem material.
            SORT lt_collect_543_batch BY material.
            LOOP AT lt_posub ASSIGNING FIELD-SYMBOL(<fs_posub>).
              CLEAR:<fs_posub>-withdrawnquantity.
              READ TABLE lt_collect_543 INTO ls_collect_543 WITH KEY purchaseorder = <fs_posub>-purchaseorder
                                                                     purchaseorderitem = <fs_posub>-purchaseorderitem
                                                                     material = <fs_posub>-material BINARY SEARCH.
              IF sy-subrc = 0.
                <fs_posub>-withdrawnquantity = ls_collect_543-withdrawnquantity.
              ENDIF.
              IF <fs_posub>-requiredquantity - <fs_posub>-withdrawnquantity > 0.
                lv_543_flag = 'X'.
              ENDIF.
            ENDLOOP.
*              IF lv_543_flag = 'X'.
            "委外订单组件消耗不为空

*              ENDIF.

            "O库存
            SELECT a~plant,
                   a~product,
                   c~lastgoodsreceiptdate,
                   a~batch,
                   a~matlwrhsstkqtyinmatlbaseunit
              FROM i_stockquantitycurrentvalue_2( p_displaycurrency = 'CNY' ) WITH PRIVILEGED ACCESS AS a
              JOIN @lt_posub AS b ON a~plant   = b~plant
                                 AND a~product = b~material
                                 AND a~supplier = b~supplier
              JOIN i_batchdistinct WITH PRIVILEGED ACCESS AS c ON a~product = c~material
                                                              AND a~batch = c~batch
             WHERE a~inventoryspecialstocktype = 'O'
               AND a~valuationareatype = '1'
               AND a~matlwrhsstkqtyinmatlbaseunit > 0
              INTO TABLE @DATA(lt_stock).
            SORT lt_stock BY plant product lastgoodsreceiptdate ASCENDING.

            IF ls_tmp_item-zzlast = 'X' AND ls_item-goodsmovementtype = '121'.
              lt_zzlast_pur = VALUE #( BASE lt_zzlast_pur
                                       ( purchaseorder = ls_item-purchaseorder
                                         purchaseorderitem = ls_item-purchaseorderitem
                                         iscompletelydelivered = 'X' )  ).

              ls_item-materialdocumentline = lv_parent_id.
              ls_data-goodsmovementcode = '07'.
              ls_item-goodsmovementrefdoctype = 'O'.
              IF lv_543_flag = 'X'.
                ls_item-quantityinentryunit = '0'.
              ENDIF.
              SELECT SINGLE plant
                       FROM i_purchaseorderitemapi01 WITH PRIVILEGED ACCESS
                      WHERE purchaseorder = @ls_item-purchaseorder
                        AND purchaseorderitem = @ls_item-purchaseorderitem
                       INTO @ls_item-plant.
              LOOP AT lt_posub INTO DATA(ls_posub).
                CLEAR:ls_sub,lv_quantity.
                ls_sub-purchaseorder  =   ls_item-purchaseorder.
                ls_sub-purchaseorderitem  = ls_item-purchaseorderitem.
                ls_sub-goodsmovementtype = '543'.  " 移动类型
                ls_sub-material = ls_posub-material. " 物料号
                ls_sub-inventoryspecialstocktype = 'O'. " 特殊库存
                ls_sub-supplier = ls_posub-supplier.
                ls_sub-plant = ls_posub-plant.
                "ls_sub-batch = '20241130AA'.
                lv_quantity = ls_posub-requiredquantity - ls_posub-withdrawnquantity."剩余需求数量
*                ls_sub-quantityinentryunit = ls_posub-requiredquantity - ls_posub-withdrawnquantity.
                IF lv_quantity <= 0.
*                  ls_sub-quantityinentryunit = 0.
*                  CONDENSE  ls_sub-quantityinentryunit NO-GAPS.
*                  ls_sub-materialdocumentparentline = lv_parent_id. " 父项目编码
*                  lv_line_id = lv_line_id + 1. " 子项目编号
*                  ls_sub-materialdocumentline = lv_line_id.
*                  APPEND ls_sub TO lt_item.
                  CONTINUE.
                ENDIF.
                READ TABLE lt_stock TRANSPORTING NO FIELDS WITH KEY plant = ls_posub-plant
                                                                    product = ls_posub-material BINARY SEARCH.
                IF sy-subrc = 0.
                  LOOP AT lt_stock INTO DATA(ls_stock) FROM sy-tabix.
                    IF ls_stock-plant = ls_posub-plant AND ls_stock-product = ls_posub-material.
                      lv_remain = lv_quantity - ls_stock-matlwrhsstkqtyinmatlbaseunit.
                      IF lv_remain > 0.
                        ls_sub-batch = ls_stock-batch.
                        ls_sub-quantityinentryunit = ls_stock-matlwrhsstkqtyinmatlbaseunit.
                        CONDENSE  ls_sub-quantityinentryunit NO-GAPS.
                        ls_sub-materialdocumentparentline = lv_parent_id. " 父项目编码
                        lv_line_id = lv_line_id + 1. " 子项目编号
                        ls_sub-materialdocumentline = lv_line_id.
                        APPEND ls_sub TO lt_item.
                        lv_quantity = lv_remain .
                      ELSE.
                        ls_sub-batch = ls_stock-batch.
                        ls_sub-quantityinentryunit = lv_quantity.
                        CONDENSE  ls_sub-quantityinentryunit NO-GAPS.
                        lv_quantity = 0.
                        ls_sub-materialdocumentparentline = lv_parent_id. " 父项目编码
                        lv_line_id = lv_line_id + 1. " 子项目编号
                        ls_sub-materialdocumentline = lv_line_id.
                        APPEND ls_sub TO lt_item.
                        EXIT.
                      ENDIF.

                    ELSE.
                      EXIT.
                    ENDIF.
                  ENDLOOP.
                  IF lv_quantity > 0.
                    o_resp-msgty = 'E'.
                    o_resp-msgtx = |委外订单行{ ls_item-purchaseorder }-{ ls_item-purchaseorderitem }组件{ ls_posub-material }缺少非限制O库存数量{ lv_quantity }| .
                    EXIT.
                  ENDIF.
                ENDIF.
*              ls_sub-quantityinentryunit = ls_posub-requiredquantity - ls_posub-withdrawnquantity.
*              CONDENSE ls_sub-quantityinentryunit NO-GAPS.
*
*              ls_sub-materialdocumentparentline = lv_parent_id. " 父项目编码
*              lv_line_id = lv_line_id + 1. " 子项目编号
*              ls_sub-materialdocumentline = lv_line_id.
*              APPEND ls_sub TO lt_item.

              ENDLOOP.
            ELSEIF ls_item-goodsmovementtype = '543'.
              "121
              ls_data-goodsmovementcode = '07'.
              ls_item-goodsmovementrefdoctype = 'O'.
              ls_item-goodsmovementtype = '121'.
              ls_item-material = ls_tmp_item-manufacturingorderitem.
              ls_item-quantityinentryunit = '0'.
              CLEAR:ls_item-entryunit,ls_item-storagelocation.
              SELECT SINGLE b~unitofmeasure_e
                       FROM i_purchaseorderitemtp_2 WITH PRIVILEGED ACCESS AS a
                       INNER JOIN i_unitofmeasure WITH PRIVILEGED ACCESS AS b
                         ON a~purchaseorderquantityunit = b~unitofmeasure
                      WHERE purchaseorder = @ls_item-purchaseorder
                        AND purchaseorderitem = @ls_item-purchaseorderitem
                       INTO @ls_item-entryunit.
              lv_parent_id = lv_line_id.
              ls_item-materialdocumentline = lv_parent_id.
              "543
              CLEAR:ls_sub,lv_quantity.
              READ TABLE lt_posub INTO ls_posub WITH KEY purchaseorder = ls_item-purchaseorder
                                                         purchaseorderitem = ls_item-purchaseorderitem
                                                         material = lv_mater18.
              IF sy-subrc = 0.
                ls_sub-purchaseorder  =   ls_item-purchaseorder.
                ls_sub-purchaseorderitem  = ls_item-purchaseorderitem.
                ls_sub-goodsmovementtype = '543'.  " 移动类型
                ls_sub-material = ls_posub-material. " 物料号
                ls_sub-inventoryspecialstocktype = 'O'. " 特殊库存
                ls_sub-supplier = ls_posub-supplier.
                ls_sub-plant = ls_posub-plant.
                lv_quantity = ls_tmp_item-quantityinentryunit.
                READ TABLE lt_stock TRANSPORTING NO FIELDS WITH KEY plant = ls_posub-plant
                                                                  product = ls_posub-material BINARY SEARCH.
                IF sy-subrc = 0.
                  LOOP AT lt_stock INTO ls_stock FROM sy-tabix.
                    IF ls_stock-plant = ls_posub-plant AND ls_stock-product = ls_posub-material.
                      lv_remain = lv_quantity - ls_stock-matlwrhsstkqtyinmatlbaseunit.
                      IF lv_remain > 0.
                        ls_sub-batch = ls_stock-batch.
                        ls_sub-quantityinentryunit = ls_stock-matlwrhsstkqtyinmatlbaseunit.
                        CONDENSE  ls_sub-quantityinentryunit NO-GAPS.
                        ls_sub-materialdocumentparentline = lv_parent_id. " 父项目编码
                        lv_line_id = lv_line_id + 1. " 子项目编号
                        ls_sub-materialdocumentline = lv_line_id.
                        APPEND ls_sub TO lt_item.
                        lv_quantity = lv_remain .
                      ELSE.
                        ls_sub-batch = ls_stock-batch.
                        ls_sub-quantityinentryunit = lv_quantity.
                        CONDENSE  ls_sub-quantityinentryunit NO-GAPS.
                        lv_quantity = 0.
                        ls_sub-materialdocumentparentline = lv_parent_id. " 父项目编码
                        lv_line_id = lv_line_id + 1. " 子项目编号
                        ls_sub-materialdocumentline = lv_line_id.
                        APPEND ls_sub TO lt_item.
                        EXIT.
                      ENDIF.

                    ELSE.
                      EXIT.
                    ENDIF.
                  ENDLOOP.
                  IF lv_quantity > 0.
                    o_resp-msgty = 'E'.
                    o_resp-msgtx = |委外订单行{ ls_item-purchaseorder }-{ ls_item-purchaseorderitem }组件{ ls_posub-material }缺少非限制O库存数量{ lv_quantity }| .
                    EXIT.
                  ENDIF.
                ENDIF.
              ELSE.
                o_resp-msgty = 'E'.
                o_resp-msgtx = |委外订单{ ls_tmp_item-manufacturingorder }产品{ ls_tmp_item-manufacturingorderitem }|
                && |组件{ ls_tmp_item-material }在SAP不存在| .
                EXIT.
              ENDIF.
            ELSEIF ls_item-goodsmovementtype = '544'.
              "121
              ls_data-goodsmovementcode = '07'.
              ls_item-goodsmovementrefdoctype = 'O'.
              ls_item-goodsmovementtype = '121'.
              ls_item-material = ls_tmp_item-manufacturingorderitem.
              ls_item-quantityinentryunit = '0'.
              lv_parent_id = lv_line_id.
              ls_item-materialdocumentline = lv_parent_id.
              CLEAR:ls_item-entryunit,ls_item-storagelocation.
              SELECT SINGLE b~unitofmeasure_e
                       FROM i_purchaseorderitemtp_2 WITH PRIVILEGED ACCESS AS a
                       INNER JOIN i_unitofmeasure WITH PRIVILEGED ACCESS AS b
                         ON a~purchaseorderquantityunit = b~unitofmeasure
                      WHERE purchaseorder = @ls_item-purchaseorder
                        AND purchaseorderitem = @ls_item-purchaseorderitem
                       INTO @ls_item-entryunit.
              "544
              CLEAR:ls_sub,lv_quantity.
              READ TABLE lt_posub INTO ls_posub WITH KEY purchaseorder = ls_item-purchaseorder
                                                         purchaseorderitem = ls_item-purchaseorderitem
                                                         material = lv_mater18.
              IF sy-subrc = 0.
                ls_sub-purchaseorder  =   ls_item-purchaseorder.
                ls_sub-purchaseorderitem  = ls_item-purchaseorderitem.
                ls_sub-goodsmovementtype = '544'.  " 移动类型
                ls_sub-material = ls_posub-material. " 物料号
                ls_sub-inventoryspecialstocktype = 'O'. " 特殊库存
                ls_sub-supplier = ls_posub-supplier.
                ls_sub-plant = ls_posub-plant.
                lv_quantity = ls_tmp_item-quantityinentryunit.
                READ TABLE lt_collect_543_batch TRANSPORTING NO FIELDS WITH KEY material = ls_posub-material BINARY SEARCH.
                IF sy-subrc = 0.
                  LOOP AT lt_collect_543_batch INTO ls_collect_543_batch FROM sy-tabix.
                    IF ls_collect_543_batch-plant = ls_posub-plant AND ls_collect_543_batch-material = ls_posub-material.
                      lv_remain = lv_quantity - ls_collect_543_batch-withdrawnquantity.
                      IF lv_remain > 0.
                        ls_sub-batch = ls_collect_543_batch-batch.
                        ls_sub-quantityinentryunit = ls_collect_543_batch-withdrawnquantity.
                        CONDENSE  ls_sub-quantityinentryunit NO-GAPS.
                        ls_sub-materialdocumentparentline = lv_parent_id. " 父项目编码
                        lv_line_id = lv_line_id + 1. " 子项目编号
                        ls_sub-materialdocumentline = lv_line_id.
                        APPEND ls_sub TO lt_item.
                        lv_quantity = lv_remain .
                      ELSE.
                        ls_sub-batch = ls_collect_543_batch-batch.
                        ls_sub-quantityinentryunit = lv_quantity.
                        CONDENSE  ls_sub-quantityinentryunit NO-GAPS.
                        lv_quantity = 0.
                        ls_sub-materialdocumentparentline = lv_parent_id. " 父项目编码
                        lv_line_id = lv_line_id + 1. " 子项目编号
                        ls_sub-materialdocumentline = lv_line_id.
                        APPEND ls_sub TO lt_item.
                        EXIT.
                      ENDIF.

                    ELSE.
                      EXIT.
                    ENDIF.
                  ENDLOOP.
                  IF lv_quantity > 0.
                    o_resp-msgty = 'E'.
                    o_resp-msgtx = |委外订单行{ ls_item-purchaseorder }-{ ls_item-purchaseorderitem }组件{ ls_posub-material }|
                    && |缺少已消耗O库存数量{ lv_quantity },无法退回| .
                    EXIT.
                  ENDIF.
                ENDIF.
              ELSE.
                o_resp-msgty = 'E'.
                o_resp-msgtx = |委外订单{ ls_tmp_item-manufacturingorder }产品{ ls_tmp_item-manufacturingorderitem }|
                && |组件{ ls_tmp_item-material }在SAP不存在| .
                EXIT.
              ENDIF.
            ENDIF.
          ENDIF.

        ENDIF.
      WHEN '201' OR '202' OR 'Z01' OR 'Z02' OR 'Z05' OR 'Z06' ."成本中心领料/冲销,会议领用
        ls_data-goodsmovementcode = '03'.
        CLEAR:ls_item-issuingorreceivingstorageloc,ls_item-issgorrcvgbatch.
      WHEN '711'  OR '712' ."盘盈/盘亏
        ls_data-goodsmovementcode = '03'.
      WHEN '551' ."报废
        ls_data-goodsmovementcode = '03'.
      WHEN '261' OR '531'."副产品收货."生产订单投料
        ls_data-goodsmovementcode = '03'.
        "获取预留
        IF ls_item-reservation  IS INITIAL.
          SELECT SINGLE
                 b~reservation,
                 b~reservationitem
            FROM i_reservationdocumentheader WITH PRIVILEGED ACCESS AS a
            JOIN i_reservationdocumentitem WITH PRIVILEGED ACCESS AS b ON a~reservation = b~reservation
           WHERE a~orderid = @ls_item-manufacturingorder
             AND b~product = @ls_tmp_item-material
            INTO (@ls_item-reservation, @ls_item-reservationitem ).
        ENDIF.
      WHEN '262' OR '532'.
        ls_data-goodsmovementcode = '03'.
      WHEN '311' ."库存调拨
        ls_data-goodsmovementcode = '04'.
      WHEN '309' ."物料转物料
        ls_data-goodsmovementcode = '04'.
      WHEN '122'."原采购退货
        IF ls_item-delivery IS NOT INITIAL .
          ls_data-goodsmovementcode = '01'.
          ls_item-goodsmovementrefdoctype = 'B'.
        ENDIF.

      WHEN '221' OR '222'."研发项目领料
        ls_data-goodsmovementcode = '03'.
        CLEAR:ls_item-issuingorreceivingstorageloc,ls_item-issgorrcvgbatch.
      WHEN '501'."无采购订单收货
        ls_data-goodsmovementcode = '01'.
        gv_wmsflag = abap_true.
        ls_item-materialdocumentitemtext = lv_tabix.
        ls_tmp_item-tabix = lv_tabix.
        IF strlen( ls_tmp_item-zzwmsbatch ) <= 10.
          ls_item-batch = ls_tmp_item-zzwmsbatch.
        ENDIF.
      WHEN '541'."发货到转包库存（供应商库存）
        ls_data-goodsmovementcode = '06'.
    ENDCASE.
*    "订单单位
*    IF ls_item-entryunit IS  INITIAL.
*      SELECT SINGLE baseunit
*        FROM i_product WITH PRIVILEGED ACCESS
*       WHERE product = @ls_tmp_item-material
*        INTO @ls_item-entryunit.
*    ENDIF.
    ls_item-manufacturedate = zzcl_comm_tool=>date2iso( ls_tmp_item-manufacturedate ).
    ls_item-shelflifeexpirationdate = zzcl_comm_tool=>date2iso( ls_tmp_item-shelflifeexpirationdate ).
    APPEND ls_item TO lt_item.

    APPEND ls_tmp_item TO gt_item.
  ENDLOOP.

  IF o_resp-msgty  = 'E'.
    DATA(lv_date1) = cl_abap_context_info=>get_system_date( ).
    DATA(lv_time) = cl_abap_context_info=>get_system_time( ).
    LOOP AT ls_tmp-req-item INTO ls_tmp_item.
      CLEAR:ls_zztmm_0006.
      DATA(lv_tabix_1) = sy-tabix.
      MOVE-CORRESPONDING ls_tmp-req-head TO ls_zztmm_0006.
      MOVE-CORRESPONDING ls_tmp_item TO ls_zztmm_0006.
      ls_zztmm_0006-outbillno = ls_zztmm_0006-materialdocumentheadertext.
      ls_zztmm_0006-outbillitemno = lv_tabix_1.
      TRY.
          ls_zztmm_0006-uuid16 = cl_system_uuid=>create_uuid_x16_static( ).
        CATCH cx_uuid_error.
      ENDTRY.
      ls_zztmm_0006-created_date = lv_date1.
      ls_zztmm_0006-created_time = lv_time.
      ls_zztmm_0006-created_by   = lv_user.
      ls_zztmm_0006-flag = o_resp-msgty.
      ls_zztmm_0006-msg = o_resp-msgtx.
      APPEND ls_zztmm_0006 TO lt_zztmm_0006.
    ENDLOOP.
    MODIFY zztmm_0006 FROM TABLE @lt_zztmm_0006.
    COMMIT WORK.
    RETURN.
  ENDIF.

  IF lt_item_zt3[] IS NOT INITIAL.
    DATA:ls_req_zt3  TYPE zzs_sdi003_req,
         ls_resp_zt3 TYPE zzs_rest_out,
         lv_pur_ww   TYPE ebeln.
    ls_req_zt3-fsysid = 'WMS'.
    ls_req_zt3-znumb = 'SD004'.
    ls_req_zt3-req-head-actualgoodsmovementdate = i_req-req-head-postingdate.
    ls_req_zt3-req-head-deliverydocumentbysupplier = i_req-req-head-materialdocumentheadertext.
    LOOP AT lt_item_zt3 ASSIGNING FIELD-SYMBOL(<fs_item_zt3>).
      SELECT SINGLE a~*
           FROM i_purchaseorderhistoryapi01 WITH PRIVILEGED ACCESS AS a
           INNER JOIN i_materialdocumentitem_2 WITH PRIVILEGED ACCESS AS b
             ON a~purchasinghistorydocument = b~materialdocument
            AND a~purchasinghistorydocumentyear = b~materialdocumentyear
            AND a~purchasinghistorydocumentitem = b~materialdocumentitem
          WHERE a~purchaseorder = @<fs_item_zt3>-purchaseorder
            AND a~purchaseorderitem = @<fs_item_zt3>-purchaseorderitem
            AND a~purchasinghistorydocumenttype = '6'
            AND a~goodsmovementtype = '643'
            AND a~documentreferenceid = @i_req-req-head-materialdocumentheadertext
            AND b~goodsmovementiscancelled = ''
            AND b~reversedmaterialdocument = ''
           INTO @DATA(ls_purchaseorderhistoryapi01).
      IF sy-subrc = 0.
        CONTINUE.
      ENDIF.
      "WMS送货单无对应采购订单交货单发货记录则需先创建交货单发货过账
      APPEND INITIAL LINE TO ls_req_zt3-req-item ASSIGNING FIELD-SYMBOL(<fs_req_item_zt3>).
      <fs_req_item_zt3>-referencesddocument = <fs_item_zt3>-purchaseorder.
      <fs_req_item_zt3>-referencesddocumentitem = <fs_item_zt3>-purchaseorderitem.
      <fs_req_item_zt3>-storagelocation = '1003'.
      <fs_req_item_zt3>-batch = <fs_item_zt3>-batch.
      <fs_req_item_zt3>-deliveryquantityunit = <fs_item_zt3>-entryunit.
      <fs_req_item_zt3>-actualdeliveryquantity = <fs_item_zt3>-quantityinentryunit.
    ENDLOOP.
    IF ls_req_zt3-req-item[] IS NOT INITIAL.
      "WMS送货单无交货单发货记录则先创建交货单发货过账
      CALL FUNCTION 'ZZFM_SD_003'
        EXPORTING
          i_req  = ls_req_zt3
        IMPORTING
          o_resp = ls_resp_zt3.
      IF ls_resp_zt3-msgty = 'E'.
        o_resp-msgty = 'E'.
        o_resp-msgtx = |公司间订单交货单创建及发货过账失败:{ ls_resp_zt3-msgtx }| .
        RETURN.
      ENDIF.
    ENDIF.

  ENDIF.

  ls_data-to_materialdocumentitem-results = lt_item.

*&---接口HTTP 链接调用
  TRY.
      DATA(lo_http_client) = cl_web_http_client_manager=>create_by_http_destination( lo_dest ).
      DATA(lo_request) = lo_http_client->get_http_request(   ).
      lo_http_client->enable_path_prefix( ).

      DATA(lv_uri_path) = |/API_MATERIAL_DOCUMENT_SRV/A_MaterialDocumentHeader?sap-language=zh|.
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
                materialdocument     TYPE string,
                materialdocumentyear TYPE string,
              END OF ty_heads,
              BEGIN OF ty_ress,
                d TYPE ty_heads,
              END OF  ty_ress.
        DATA:ls_ress TYPE ty_ress.
        /ui2/cl_json=>deserialize( EXPORTING json  = lv_res
                                    CHANGING data  = ls_ress ).

        o_resp-msgty  = 'S'.
        o_resp-msgtx  = 'success'.
        o_resp-sapnum = ls_ress-d-materialdocument.
        o_resp-sapnum = |{ ls_ress-d-materialdocument }-{ ls_ress-d-materialdocumentyear }| .
        gv_mblnr = ls_ress-d-materialdocument.
        gv_year = ls_ress-d-materialdocumentyear.
        IF lt_zztmm_0005[] IS NOT INITIAL.
          SELECT *
          FROM i_materialdocumentitem_2 WITH PRIVILEGED ACCESS
         WHERE materialdocument = @gv_mblnr
           AND materialdocumentyear = @gv_year
          INTO TABLE @DATA(lt_materialdocumentitem_2).
          lv_date1 = cl_abap_context_info=>get_system_date( ).
          lv_time = cl_abap_context_info=>get_system_time( ).
          LOOP AT lt_zztmm_0005 ASSIGNING FIELD-SYMBOL(<fs_zztmm_0005>).
            <fs_zztmm_0005>-created_date = lv_date1.
            <fs_zztmm_0005>-created_time = lv_time.
            <fs_zztmm_0005>-created_by   = lv_user.
            <fs_zztmm_0005>-materialdocument = gv_mblnr.
            <fs_zztmm_0005>-materialdocumentyear = gv_year.
            READ TABLE lt_materialdocumentitem_2 INTO DATA(ls_materialdocumentitem_2) WITH KEY materialdocumentitemtext = <fs_zztmm_0005>-materialdocumentitemtext.
            IF sy-subrc = 0.
              <fs_zztmm_0005>-materialdocumentitem = ls_materialdocumentitem_2-materialdocumentitem.
              <fs_zztmm_0005>-inventoryusabilitycode = ls_materialdocumentitem_2-inventoryusabilitycode.
              <fs_zztmm_0005>-batch = ls_materialdocumentitem_2-batch.
            ENDIF.
          ENDLOOP.
          IF lt_zztmm_0005[] IS NOT INITIAL.
            MODIFY zztmm_0005 FROM TABLE @lt_zztmm_0005.
            COMMIT WORK AND WAIT.
          ENDIF.
        ENDIF.
        WAIT UP TO 1 SECONDS.

        "更改批次
        CALL FUNCTION 'ZZFM_MM_001_BATCH'.

        IF lt_zzlast_pur[] IS NOT INITIAL.
          DATA:lo_util        TYPE REF TO zcl_mmi003_util.
          CREATE OBJECT lo_util.
          "采购订单行打交货完成标识
          lo_util->deal_0002( CHANGING lt_req = lt_zzlast_pur ).
        ENDIF.
      ELSE.
        DATA:ls_rese TYPE zzs_odata_fail.
        /ui2/cl_json=>deserialize( EXPORTING json  = lv_res
                                    CHANGING data  = ls_rese ).
        o_resp-msgty = 'E'.
        o_resp-msgtx = ls_rese-error-message-value .
        IF ls_rese-error-innererror-errordetails[] IS NOT INITIAL.
          LOOP AT ls_rese-error-innererror-errordetails[] ASSIGNING FIELD-SYMBOL(<fs_error_detail>) WHERE severity = 'error'.
            o_resp-msgtx = |{ o_resp-msgtx }/{ <fs_error_detail>-message }|.
          ENDLOOP.
        ENDIF.
      ENDIF.
    CATCH cx_web_http_client_error INTO DATA(lx_web_http_client_error).
      o_resp-msgty = 'E'.
      o_resp-msgtx = lx_web_http_client_error->get_longtext( ) .
  ENDTRY.
  lv_date1 = cl_abap_context_info=>get_system_date( ).
  lv_time = cl_abap_context_info=>get_system_time( ).
  LOOP AT ls_tmp-req-item INTO ls_tmp_item.
    CLEAR:ls_zztmm_0006.
    lv_tabix_1 = sy-tabix.
    MOVE-CORRESPONDING ls_tmp-req-head TO ls_zztmm_0006.
    MOVE-CORRESPONDING ls_tmp_item TO ls_zztmm_0006.
    ls_zztmm_0006-outbillno = ls_zztmm_0006-materialdocumentheadertext.
    ls_zztmm_0006-outbillitemno = lv_tabix_1.
    TRY.
        ls_zztmm_0006-uuid16 = cl_system_uuid=>create_uuid_x16_static( ).
      CATCH cx_uuid_error.
    ENDTRY.
    ls_zztmm_0006-created_date = lv_date1.
    ls_zztmm_0006-created_time = lv_time.
    ls_zztmm_0006-created_by   = lv_user.
    ls_zztmm_0006-materialdocument = gv_mblnr.
    ls_zztmm_0006-materialdocumentyear = gv_year.
    ls_zztmm_0006-flag = o_resp-msgty.
    ls_zztmm_0006-msg = o_resp-msgtx.
    APPEND ls_zztmm_0006 TO lt_zztmm_0006.
  ENDLOOP.
  MODIFY zztmm_0006 FROM TABLE @lt_zztmm_0006.
  COMMIT WORK.
ENDFUNCTION.
