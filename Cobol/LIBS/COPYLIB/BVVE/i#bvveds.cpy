      *----------------------------------------------------------------*
      *  I#BVVEDS - MANUTENCAO DE DOMICILIO BANCARIO - RETORNO         *
      *  CODIGO MENSAGEM CIP : SCG0013E                                *
      *  REQUISICAO BAIXA MANUTENCAO DE DOMICILIO BANCARIO             *
      *  FORMATO DATAS: AAAAMMDD                                       *
      *----------------------------------------------------------------*

       01  BVVEDS-SCG0013E.
           03 BVVEDS-COD-MSG           PIC X(009).
           03 BVVEDS-COD-MSG-E         PIC X(008).
           03 BVVEDS-NUM-CTRL-IF       PIC X(020).
           03 BVVEDS-NUM-CTRL-IF-E     PIC X(008).
           03 BVVEDS-ISPB-IF           PIC X(008).
           03 BVVEDS-ISPB-IF-E         PIC X(008).
           03 BVVEDS-NUM-IDENTC-SCG    PIC 9(017).
           03 BVVEDS-NUM-IDENTC-SCG-E  PIC X(008).
           03 BVVEDS-TP-BAIXA-DOMCL    PIC X(001).
           03 BVVEDS-TP-BAIXA-DOMCL-E  PIC X(008).
           03 BVVEDS-DT-MOVTO          PIC 9(008).
           03 BVVEDS-DT-MOVTO-E        PIC X(008).
