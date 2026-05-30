      *----------------------------------------------------------------*
      *  I#BVVEDB - MANUTENCAO DE DOMICILIO BANCARIO                   *
      *  CODIGO MENSAGEM CIP : SCG0005R1                               *
      *  RESPOSTA CONSULTA REGISTRO DE MANUTENCAO DE DOMICILIO BANCARIO*
      *  FORMATO DATAS: AAAAMMDD                                       *
      *  FORMATO DATA/HORA: AAAAMMDDHHMMSSMMMMMM                       *
      *----------------------------------------------------------------*

       01  BVVEDB-SCG0005R1.
           03 BVVEDB-COD-MSG           PIC X(009).
           03 BVVEDB-NUM-CTRL-IF       PIC X(020).
           03 BVVEDB-ISPB-IF           PIC X(008).
           03 BVVEDB-TP-MANUT          PIC X(001).
           03 BVVEDB-VARIAVEL          PIC X(18095).

       03  BVVEDB-GRP-DOMCL-BASE.
           05 BVVEDB-CNPJ-BASE-CLI     PIC X(008).
           05 BVVEDB-PROP-ESQM-BASE    PIC X(003).
           05 BVVEDB-TP-PRODT-BASE     PIC X(001).
           05 BVVEDB-INDR-BLOQ-BASE    PIC X(001).

       03 BVVEDB-GRP-DOMCL-COMPL.
           05 BVVEDB-FILLER            PIC X(006).
           05 BVVEDB-DOMCL-DADOS.
            07 BVVEDB-NUM-IDENTC-SCG    PIC X(017).
            07 BVVEDB-TP-PESSOA-CLI     PIC X(001).
            07 BVVEDB-CNPJ-CPF-CLI      PIC 9(014).
            07 BVVEDB-COD-PROPT-COMPL   PIC X(003).
            07 BVVEDB-TP-PRODT-COMPL    PIC X(001).
            07 BVVEDB-DTHR-MANUT        PIC X(014).
            07 BVVEDB-INDR-BLOQ-COMPL   PIC X(001).
            07 BVVEDB-CLI-PROPRIO.
               09 BVVEDB-AG-CLI            PIC 9(004).
               09 BVVEDB-CT-CLI            PIC 9(013).
               09 BVVEDB-PERC-GAR          PIC 9(005).
               09 BVVEDB-DTINI-VALIDD      PIC 9(008).
               09 BVVEDB-DTFIM-VALIDD      PIC 9(008).
               09 BVVEDB-TP-NATUR-GAR      PIC X(001).
               09 BVVEDB-VLR-MAX-RET       PIC X(019).

       01  BVVEDB-FIXO-FINAL.
           03 BVVEDB-INDR-INF-ADCL     PIC X(001).
           03 BVVEDB-SIT-CONS          PIC 9(002).
           03 BVVEDB-TAM-ARQ           PIC X(015).
           03 BVVEDB-IDENTD-ARQ        PIC X(255).
           03 BVVEDB-DT-HR-SIT         PIC X(014).
           03 BVVEDB-DT-MOVTO          PIC 9(008).

