      *----------------------------------------------------------------*
      *  I#BVVEDA - MANUTENCAO DE DOMICILIO BANCARIO                   *
      *  CODIGO MENSAGEM CIP : SCG0005                                 *
      *  CONSULTA REGISTRO DE MANUTENCAO DE DOMICILIO BANCARIO         *
      *  FORMATO DATAS: AAAAMMDD                                       *
      *----------------------------------------------------------------*

       01  BVVEDA-SCG0005.
           03 BVVEDA-COD-MSG           PIC X(009).
           03 BVVEDA-NUM-CTRL-IF       PIC X(020).
           03 BVVEDA-ISPB-IF           PIC X(008).
           03 BVVEDA-NUM-IDENTC-SCG-X.
              05 BVVEDA-NUM-IDENTC-SCG PIC 9(017).
           03 BVVEDA-DADOS-CLI.
              05 BVVEDA-TP-PESSOA-CLI  PIC X(001).
              05 BVVEDA-CNPJ-CPF-CLI   PIC 9(014).
           03 BVVEDA-CNPJ-BASE-CLI     PIC X(008).
           03 BVVEDA-COD-PROPT-ESQM    PIC X(003).
           03 BVVEDA-TP-RET            PIC X(001).
           03 BVVEDA-DT-MOVTO          PIC 9(008).
