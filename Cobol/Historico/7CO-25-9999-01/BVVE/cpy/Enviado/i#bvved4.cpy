      *----------------------------------------------------------------*
      *  I#BVVED4 - ARQUIVO DE CONSULTA DE DOMICILIO BANCARIO- CIP     *
      *  DSNAME: CB.BACC.VON.TRAVPCON                                  *
      *----------------------------------------------------------------*

       01  BVVED4-TRAVPCON.
           03 BVVED4-CHAVE.
7C2511        05 BVVED4-CNPJ               PIC X(009).
7C2511*        05 BVVED4-CNPJ               PIC 9(009).
7C2511        05 BVVED4-FILIAL             PIC X(004).
7C2511*        05 BVVED4-FILIAL             PIC 9(004).
              05 BVVED4-NUM-CTRL-IF        PIC X(020).
              05 BVVED4-FILLER-CHV         PIC X(057).
           03 BVVED4-TIMESTAMP             PIC X(020).
           03 BVVED4-NUM-IDENTC-SCG-X.
               05 BVVED4-NUM-IDENTC-SCG    PIC 9(017).
           03 BVVED4-COD-MSG               PIC X(009).
           03 BVVED4-STATUS                PIC X(002).
           03 BVVED4-ISPB-IF               PIC X(008).
           03 BVVED4-SOLICT-RAIZ           PIC X(001).
           03 BVVED4-DADOS-CLI.
              05 BVVED4-TP-PESSOA-CLI      PIC X(001).
7C2511        05 BVVED4-CNPJ-CPF-CLI       PIC X(014).
7C2511*        05 BVVED4-CNPJ-CPF-CLI       PIC 9(014).
           03 BVVED4-CNPJ-BASE-CLI         PIC X(008).
           03 BVVED4-COD-PROPT-ESQM        PIC X(003).
           03 BVVED4-COD-BANDEIRA          PIC 9(003).
           03 BVVED4-TP-PRODT              PIC X(001).
           03 BVVED4-CLI-PROPRIO.
               05 BVVED4-AG-CLI            PIC 9(004).
               05 BVVED4-CT-CLI            PIC 9(013).
               05 BVVED4-PERC-GAR          PIC 9(005).
               05 BVVED4-DTINI-VALIDD      PIC 9(008).
               05 BVVED4-DTFIM-VALIDD      PIC 9(008).
           03 BVVED4-TP-RET                PIC X(001).
           03 BVVED4-INDR-BLOQ-DOMCL       PIC X(001).
           03 BVVED4-INDR-INF-ADCL         PIC X(001).
           03 BVVED4-SIT-CONS              PIC 9(002).
           03 BVVED4-TAM-ARQ               PIC X(015).
           03 BVVED4-IDENTD-ARQ            PIC X(040).
           03 BVVED4-DT-MOVTO              PIC 9(008).
           03 BVVED4-DTHR-MANUT            PIC X(020).
           03 BVVED4-DT-HR-SIT             PIC X(020).
           03 BVVED4-ERRO                  PIC 9(002).
           03 BVVED4-ERROS.
              05 BVVED4-COD-ERRO           PIC X(008) OCCURS 10.
           03 BVVED4-OPERADOR              PIC X(007).
           03 BVVED4-TP-MANUT              PIC X(001).
           03 BVVED4-INST-DETENTORA        PIC X(005).
           03 BVVED4-TP-NATUR-GAR          PIC X(001).
           03 BVVED4-VLR-MAX-RET           PIC 9(013)V99.
           03 FILLER                       PIC X(086).

