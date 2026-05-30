      *----------------------------------------------------------------*
      *  I#BVVED5 - ARQUIVO DE ANTECIPACOES DO CPF/CNPJ *
      *  DSNAME: CB.BACC.VON.TRAVPANT                                  *
      *----------------------------------------------------------------*

       01  BVVED5-TRAVPANT.
           03 BVVED5-CHAVE.
              05 BVVED5-NUM-IDENTC-SCG PIC 9(017).
              05 BVVED5-TIMESTAMP      PIC X(020).
              05 BVVED5-FILLER-CHV     PIC X(033).
           03 BVVED5-COD-MSG           PIC X(009).
           03 BVVED5-NUM-CTRL-CIP      PIC X(020).
           03 BVVED5-ISPB-IF           PIC X(008).
           03 BVVED5-NOM-CREDDR        PIC X(040).
           03 BVVED5-INDR-ANTEC        PIC X(001).
           03 BVVED5-QTD-CREDDR        PIC 9(009).
           03 BVVED5-QTD-RET           PIC 9(009).
           03 BVVED5-DT-HR-SCG         PIC X(014).
           03 BVVED5-DT-MOVTO          PIC 9(008).
           03 FILLER                   PIC X(112).
