      *----------------------------------------------------------------*
      *  I#BVVEDG - MANUTENCAO DE DOMICILIO BANCARIO                   *
      *  CODIGO MENSAGEM CIP : SCG0013                                 *
      *  REQUISICAO BAIXA MANUTENCAO DE DOMICILIO BANCARIO             *
      *  FORMATO DATAS: AAAAMMDD                                       *
      *----------------------------------------------------------------*

       01  BVVEDG-SCG0013.
           03 BVVEDG-COD-MSG           PIC X(009).
           03 BVVEDG-NUM-CTRL-IF       PIC X(020).
           03 BVVEDG-ISPB-IF           PIC X(008).
           03 BVVEDG-NUM-IDENTC-SCG    PIC 9(017).
           03 BVVEDG-TP-BAIXA-DOMCL    PIC X(001).
           03 BVVEDG-DT-MOVTO          PIC 9(008).
