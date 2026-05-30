      *----------------------------------------------------------------*
      *  I#BVVEDH - MANUTENCAO DE DOMICILIO BANCARIO                   *
      *  ARQUIVO ASCG008                                               *
      *  CARGA INICIAL DE MANUTENCAO DOMICILIO BANCARIO                *
      *  FORMATO DATAS: AAAAMMDD                                       *
      *  FORMATO DATA/HORA: AAAA-MM-DDTHH:MM:SS:MMMMMM                 *
      *----------------------------------------------------------------*

       01  REG-ASCG008.
           03  BVVEDH-ID-REG                PIC X(01).
           03  BVVEDH-HEADER.
               05  BVVEDH-NOME-ARQ          PIC X(050).
               05  BVVEDH-NUM-CTRL-EMIS     PIC X(020).
               05  BVVEDH-NUM-CTRL-DEST-OR  PIC X(020).
               05  BVVEDH-ISPB-EMISSOR      PIC X(008).
               05  BVVEDH-ISPB-DESTINATARIO PIC X(008).
               05  BVVEDH-DT-HR-ARQ         PIC X(019).
               05  BVVEDH-SIT-CONS          PIC X(002).
               05  BVVEDH-GRUPO-SEQ.
                   07  BVVEDH-NUM-SEQ       PIC 9(009).
                   07  BVVEDH-INDR-CONT     PIC X(001).
               05  BVVEDH-DT-REF            PIC X(008).
               05  FILLER                   PIC X(27976).
           03  BVVEDH-DETALHE REDEFINES BVVEDH-HEADER.
               05 BVVEDH-NUM-IDENTC-SCG    PIC 9(017).
               05 BVVEDH-DADOS-CLI.
                  07 BVVEDH-TP-PESSOA-CLI  PIC X(001).
                  07 BVVEDH-CNPJ-CPF-CLI   PIC 9(014).
               05 BVVEDH-COD-PROPT-ESQM    PIC X(003).
               05 BVVEDH-TP-PRODT          PIC X(001).
               05 BVVEDH-AG-CLI            PIC 9(004).
               05 BVVEDH-CT-CLI            PIC 9(013).
               05 BVVEDH-SINAL             PIC X(001).
               05 BVVEDH-PERC-GAR          PIC 9(005).
               05 BVVEDH-DTINI-VALIDD      PIC 9(008).
               05 BVVEDH-DTFIM-VALIDD      PIC 9(008).
               05 BVVEDH-DTHR-MANUT        PIC X(019).
               05 BVVEDH-VLR-MAX-RET       PIC 9(013)V99.
               05 BVVEDH-CNPJ-BASE.
                   07 BVVEDH-CNPJ-BASE-CLI PIC X(008).
                   07 BVVEDH-QTD-CNPJ      PIC 9(004).
                   07 BVVEDH-CNPJ-CLI      PIC 9(014) OCCURS 2000 TIMES.

