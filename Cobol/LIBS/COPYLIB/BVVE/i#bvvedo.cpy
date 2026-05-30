      *----------------------------------------------------------------*
      *  I#BVVEDO - MANUTENCAO DE DOMICILIO BANCARIO                   *
      *  ARQUIVO ASCG009                                               *
      *  INFORMA MODIFICACOES DE MANUTENCAO DOMICILIO BANCARIO         *
      *  FORMATO DATAS: AAAAMMDD                                       *
      *  FORMATO DATA/HORA: AAAA-MM-DDTHH:MM:SS:MMMMMM                 *
      *----------------------------------------------------------------*

       01  REG-ASCG009.
           03  BVVEDO-ID-REG                PIC X(01).
           03  BVVEDO-HEADER.
               05  BVVEDO-NOME-ARQ          PIC X(050).
               05  BVVEDO-NUM-CTRL-EMIS     PIC X(020).
               05  BVVEDO-NUM-CTRL-DEST-OR  PIC X(020).
               05  BVVEDO-ISPB-EMISSOR      PIC X(008).
               05  BVVEDO-ISPB-DESTINATARIO PIC X(008).
               05  BVVEDO-DT-HR-ARQ         PIC X(019).
               05  BVVEDO-SIT-CONS          PIC X(002).
               05  BVVEDO-GRUPO-SEQ.
                   07  BVVEDO-NUM-SEQ       PIC 9(009).
                   07  BVVEDO-INDR-CONT     PIC X(001).
               05  BVVEDO-DT-REF            PIC X(008).
               05  FILLER                   PIC X(28009).
           03  BVVEDO-DETALHE REDEFINES BVVEDO-HEADER.
               05 BVVEDO-CHAVE.
                   07 BVVEDO-CHV-TP        PIC X(001).
                   07 BVVEDO-CHV-CNPJCPF   PIC 9(009).
                   07 BVVEDO-CHV-FILIAL    PIC 9(004).
               05 BVVEDO-NUM-IDENTC-SCG    PIC 9(017).
               05 BVVEDO-TP-MANUT-DOMCL    PIC X(001).
               05 BVVEDO-DADOS-CLI         PIC X(015).
               05 FILLER REDEFINES BVVEDO-DADOS-CLI.
                   07 BVVEDO-TP-PESSOA-CLI PIC X(001).
                   07 BVVEDO-CNPJ-CPF-CLI  PIC 9(014).
               05 BVVEDO-IDENTC-CREDDR     PIC X(008).
               05 BVVEDO-TP-BAIXA-DOMCL    PIC X(001).
               05 BVVEDO-COD-PROPT-ESQM    PIC X(003).
               05 BVVEDO-TP-PRODT          PIC X(001).
               05 BVVEDO-AG-CLI            PIC 9(004).
               05 BVVEDO-CT-CLI            PIC 9(013).
               05 BVVEDO-SINAL             PIC X(001).
               05 BVVEDO-PERC-GAR          PIC 9(005).
               05 BVVEDO-DTINI-VALIDD      PIC 9(008).
               05 BVVEDO-DTFIM-VALIDD      PIC 9(008).
               05 BVVEDO-TP-NATU-GAR       PIC X(001).
               05 BVVEDO-DT-SOLICTC-DOCC   PIC X(008).
               05 BVVEDO-DTHR-MANUT        PIC X(019).
               05 BVVEDO-VLR-MAX-RET       PIC 9(013)V99.
               05 BVVEDO-CNPJ-BASE.
                   07 BVVEDO-CNPJ-BASE-CLI PIC X(008).
                   07 BVVEDO-QTD-CNPJ      PIC 9(004).
                   07 BVVEDO-CNPJ-CLI      PIC 9(014) OCCURS 2000 TIMES.

