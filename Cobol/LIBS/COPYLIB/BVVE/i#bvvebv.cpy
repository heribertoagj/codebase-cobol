      *----------------------------------------------------------------*
      *  CONSTITUICAO DE GARANTIA DE ORPAG'S POR SALDO DEVEDOR         *
      *  I#BVVEBV - ARQUIVO DE TRABALHO                                *
      *  TAMANHO: 200                                                  *
      *----------------------------------------------------------------*

       01  BVVEBV-REGISTRO.
           03 BVVEBV-AGENCIA               PIC 9(005).
           03 BVVEBV-CONTA                 PIC 9(013).
           03 BVVEBV-CONTRATO              PIC 9(017).
           03 BVVEBV-CCUSTO                PIC X(004).
           03 BVVEBV-COD-GARANTIA          PIC 9(003).
           03 BVVEBV-DT-VCTO-FIM           PIC 9(008).
           03 BVVEBV-VLR-GARNT             PIC 9(013)V99.
           03 BVVEBV-DESC-BAND             PIC X(015).
           03 BVVEBV-AGENCIA-CONTRATO      PIC 9(005).
           03 BVVEBV-CONTA-CONTRATO        PIC 9(013).
           03 BVVEBV-CARTEIRA              PIC X(003).
           03 BVVEBV-AREAS-RESERVADAS.
              05 BVVEBV-VLR-DCOM           PIC 9(013)V99.
              05 BVVEBV-DT-INI-CONTR       PIC 9(008).
              05 BVVEBV-CNPJ               PIC 9(009).
              05 BVVEBV-FILIAL             PIC 9(004).
              05 BVVEBV-CTRL               PIC 9(002).
           03 BVVEBV-AREA-COSA.
              05 BVVEBV-SALDO              PIC S9(15)V99.
              05 BVVEBV-RAZAO              PIC 9(003).
              05 BVVEBV-SUB-GRUPO          PIC 9(003).
           03 BVVEBV-VLR-A-BLOQ            PIC 9(013)V99.
           03 BVVEBV-VLR-BLOQADO           PIC 9(013)V99.
           03 BVVEBV-COD-BAND              PIC 9(003).
           03 FILLER                       PIC X(005).
      *    DATA: AAAAMMDD
      *    BVVEBV-COD-BAND: BANDEIRA CORPORATIVA VISAO 2
