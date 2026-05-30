      *----------------------------------------------------------------*
      *  I#BVVED3 - ARQUIVO DE MANUTENCOES DE DOMICILIO BANCARIO *
      *  DSNAME: CB.BACC.VON.TRAVPMAN                                  *
      *----------------------------------------------------------------*

       01  BVVED3-TRAVPMAN.
           03 BVVED3-CHAVE.
7C2511        05 BVVED3-CNPJ               PIC X(009).
7C2511*        05 BVVED3-CNPJ               PIC 9(009).
7C2511        05 BVVED3-FILIAL             PIC X(004).
7C2511*        05 BVVED3-FILIAL             PIC 9(004).
              05 BVVED3-NUM-CTRL-IF        PIC X(020).
              05 BVVED3-FILLER-CHV         PIC X(057).
           03 BVVED3-NUM-IDENTC-SCG        PIC 9(017).
           03 BVVED3-COD-MSG               PIC X(009).
           03 BVVED3-STATUS                PIC X(002).
           03 BVVED3-TIMESTAMP             PIC X(020).
           03 BVVED3-ISPB-IF               PIC X(008).
           03 BVVED3-NUM-CTRL-CIP          PIC X(020).
           03 BVVED3-SOLICT-RAIZ           PIC X(001).
           03 BVVED3-DADOS-CLI.
              05 BVVED3-TP-PESSOA-CLI      PIC X(001).
7C2511        05 BVVED3-CNPJ-CPF-CLI       PIC X(014).
7C2511*        05 BVVED3-CNPJ-CPF-CLI       PIC 9(014).
7C2511     03 BVVED3-CNPJ-BASE-CLI         PIC X(008).
7C2511*     03 BVVED3-CNPJ-BASE-CLI         PIC 9(008).
           03 BVVED3-COD-PROPT-ESQM        PIC X(003).
           03 BVVED3-COD-BANDEIRA          PIC 9(003).
           03 BVVED3-TP-PRODT              PIC X(001).
           03 BVVED3-NUM-COD-IF            PIC X(003).
           03 BVVED3-AG-CLI                PIC 9(004).
           03 BVVED3-CT-CLI                PIC 9(013).
           03 BVVED3-PERC-GAR              PIC 9(003)V99.
           03 BVVED3-DTINI-VALIDD          PIC 9(008).
           03 BVVED3-DTFIM-VALIDD          PIC 9(008).
           03 BVVED3-TP-NATU-GAR           PIC X(001).
           03 BVVED3-TP-BAIXA-DOMCL        PIC X(001).
           03 BVVED3-TP-MANUT-DOMCL        PIC X(001).
           03 BVVED3-IDENTC-CREDDR         PIC X(008).
           03 BVVED3-NOM-CREDDR            PIC X(040).
           03 BVVED3-INDR-ANTEC            PIC X(001).
           03 BVVED3-QTD-CREDDR            PIC 9(009).
           03 BVVED3-QTD-RET               PIC 9(009).
           03 BVVED3-DTHR-SCG              PIC X(020).
           03 BVVED3-DT-MOVTO              PIC 9(008).
           03 BVVED3-DTHR-MANUT            PIC X(020).
           03 BVVED3-DT-SOLICTC-DOCC       PIC 9(008).
           03 BVVED3-ERRO                  PIC 9(002).
           03 BVVED3-ERROS.
              05 BVVED3-COD-ERRO           PIC X(008) OCCURS 20.
           03 BVVED3-OPERADOR              PIC X(007).
           03 BVVED3-AGENCIA-OLD           PIC 9(005).
           03 BVVED3-CONTA-OLD             PIC 9(013).
           03 BVVED3-VLR-MAX-RET           PIC 9(013)V99.
           03 BVVED3-FILLER                PIC X(034).
