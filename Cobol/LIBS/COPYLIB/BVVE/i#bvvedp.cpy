      *----------------------------------------------------------------*
      *  I#BVVEDP - MANUTENCAO DE DOMICILIO BANCARIO -ERRO             *
      *  CODIGO MENSAGEM CIP : SCG0005E                                *
      *  CONSULTA REGISTRO DE MANUTENCAO DE DOMICILIO BANCARIO         *
      *  FORMATO DATAS: AAAAMMDD                                       *
      *----------------------------------------------------------------*

       01  BVVEDP-SCG0005E.
           03 BVVEDP-COD-MSG            PIC X(009).
           03 BVVEDP-COD-MSG-E          PIC X(008).
           03 BVVEDP-NUM-CTRL-IF        PIC X(020).
           03 BVVEDP-NUM-CTRL-IF-E      PIC X(008).
           03 BVVEDP-ISPB-IF            PIC X(008).
           03 BVVEDP-ISPB-IF-E          PIC X(008).
           03 BVVEDP-NUM-IDENTC-SCG     PIC X(017).
           03 BVVEDP-NUM-IDENTC-SCG-E   PIC X(008).
           03 BVVEDP-DADOS-CLI.
              05 BVVEDP-TP-PESSOA-CLI   PIC X(001).
              05 BVVEDP-TP-PESSOA-CLI-E PIC X(008).
              05 BVVEDP-CNPJ-CPF-CLI    PIC 9(014).
              05 BVVEDP-CNPJ-CPF-CLI-E  PIC X(008).
           03 BVVEDP-CNPJ-BASE-CLI      PIC X(008).
           03 BVVEDP-CNPJ-BASE-CLI-E    PIC X(008).
           03 BVVEDP-COD-PROPT-ESQM     PIC X(003).
           03 BVVEDP-COD-PROPT-ESQM-E   PIC X(008).
           03 BVVEDP-TP-RET             PIC X(001).
           03 BVVEDP-TP-RET-E           PIC X(008).
           03 BVVEDP-DT-MOVTO           PIC X(008).
           03 BVVEDP-DT-MOVTO-E         PIC X(008).
