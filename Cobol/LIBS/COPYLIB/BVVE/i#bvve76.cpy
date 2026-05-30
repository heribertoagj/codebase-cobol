      *---------------------------------------------------------------*
      * BOOK DO CADASTRO DE VENDEDORES (CADELO)                       *
      * TAMANHO : 0350    FATOR DE BLOCO : 33    BLKSIZE : 11550      *
      * DSN=J.CELO.N009300.CADASTRO                                   *
      * I#BVVE76                          ULTIMA ALTERACAO : 16/05/95 *
      *---------------------------------------------------------------*
      *                                                         POS TAM

       01  B076-REGISTRO.
      *                                                         001 350
           05  B076-VENDEDOR.
      *                                                         001 011
               10  B076-VENDPREF       PIC  9(005)     COMP-3.
      *                                                         001 003
               10  B076-VENDBCO        PIC  9(005)     COMP-3.
      *                                                         004 003
               10  B076-VENDFIXO       PIC  9(001).
      *                                                         007 001
               10  B076-VENDNUM        PIC  9(007)     COMP-3.
           05  B076-BANCO              PIC  9(003)     COMP-3.
      *                                                         012 002
           05  B076-AGENCIA            PIC  9(005)     COMP-3.
      *                                                         014 003
           05  B076-RAZAO              PIC  9(005)     COMP-3.
      *                                                         017 003
           05  B076-CONTA              PIC  9(007)     COMP-3.
      *                                                         020 004
           05  B076-ZERO               PIC  9(001).
      *                                                         024 001
           05  B076-NOMVEND            PIC  X(035).
      *                                                         025 035
      *
      *    SE NAS DUAS PRIMEIRAS POSICOES DO ENDERECO DO VENDEDOR CONTER
      *    '**' INDICA QUE O MESMO ESTA CANCELADO,  NAS POSICOES 86 A 93
      *    ESTA A DATA DE CANCELAMENTO.
      *
           05  B076-ENDVEND            PIC  X(035).
      *                                                         060 035
           05  FILLER  REDEFINES  B076-ENDVEND.
      *                                                         060 002
               10  FILLER              PIC  X(024).
      *                                                         062 024
               10  B076-DATACANC.
                   15  B076-DATACANC-DD  PIC  9(002).
      *                                                         086 002
                   15  FILLER            PIC  X(001).
      *                                                         088 001
                   15  B076-DATACANC-MM  PIC  9(002).
      *                                                         089 002
                   15  FILLER            PIC  X(001).
      *                                                         091 001
                   15  B076-DATACANC-AA  PIC  9(002).
      *                                                         092 002
               10  FILLER              PIC  X(001).
      *                                                         094 001
           05  B076-CEPVEND            PIC  9(005).
      *                                                         095 005
      *    DATA ATUALIZACAO CADASTRAL  (DDMMAA)
      *
      *                                                         100 004
      *    DATA MOVIMENTO DE VALORES   (DDMMAA)
      *
           05  B076-DTMVTVAL           PIC  9(007)     COMP-3.
      *                                                         104 004
      *    VENDAS A VISTA MES ATUAL
      *
           05  B076-VENORPAV-AT        PIC  9(013)V99  COMP-3.
      *                                                         108 008
           05  B076-COMORPAV-AT        PIC  9(011)V99  COMP-3.
      *                                                         116 007
           05  B076-VENADSAV-AT        PIC  9(013)V99  COMP-3.
      *                                                         123 008
           05  B076-COMADSAV-AT        PIC  9(011)V99  COMP-3.
      *                                                         131 007
      *    VENDAS A VISTA MES POSTERIOR
      *
           05  B076-VENORPAV-POS       PIC  9(013)V99  COMP-3.
      *                                                         138 008
           05  B076-COMORPAV-POS       PIC  9(011)V99  COMP-3.
           05  B076-VENADSAV-POS       PIC  9(013)V99  COMP-3.
      *                                                         153 008
           05  B076-COMADSAV-POS       PIC  9(011)V99  COMP-3.
      *                                                         161 007
      *    VENDAS A PRAZO MES ATUAL
      *
           05  B076-VENORPAP-AT        PIC  9(013)V99  COMP-3.
      *                                                         168 008
           05  B076-COMORPAP-AT        PIC  9(011)V99  COMP-3.
      *                                                         176 007
           05  B076-VENADSAP-AT        PIC  9(013)V99  COMP-3.
      *                                                         183 008
           05  B076-COMADSAP-AT        PIC  9(011)V99  COMP-3.
      *                                                         191 007
      *    VENDAS A PRAZO MES POSTERIOR
      *
           05  B076-VENORPAP-POS       PIC  9(013)V99  COMP-3.
      *                                                         198 008
           05  B076-COMORPAP-POS       PIC  9(011)V99  COMP-3.
      *                                                         206 007
      *                                                         213 008
           05  B076-COMADSAP-POS       PIC  9(011)V99  COMP-3.
      *                                                         221 007
      *    TOTAL DAS VENDAS A VISTA
      *
           05  B076-VENTOTAV-MESANT    PIC  9(013)V99  COMP-3.
      *                                                         228 008
           05  B076-VENTOTAV-NOANO     PIC  9(013)V99  COMP-3.
      *                                                         236 008
      *    TOTAL DAS VENDAS A PRAZO
      *
           05  B076-VENTOTAP-MESANT    PIC  9(013)V99  COMP-3.
      *                                                         244 008
           05  B076-VENTOTAP-NOANO     PIC  9(013)V99  COMP-3.
      *                                                         252 008
           05  B076-TAXAS.
               10  B076-TXAVISTA.
                   15  B076-TXAVADS    PIC  9(003)V99  COMP-3.
      *                                                         260 003
                   15  B076-TXAVORPAG  PIC  9(003)V99  COMP-3.
               10  B076-TXAPRAZO.
                   15  B076-TXAPADS    PIC  9(003)V99  COMP-3.
      *                                                         266 003
                   15  B076-TXAPORPAG  PIC  9(003)V99  COMP-3.
      *                                                         269 003
      *    TIPO DE CREDITO
      *    SE N = NORMAL
      *    SE R = ROTATIVO
      *    SE F = FIXO
      *    SE V = ROTATIVO VISA
      *    SE U = FIXO VISA
      *
           05  B076-TIPCRED            PIC  X(001).
      *                                                         272 001
           05  B076-QTDIACR            PIC  9(002).
      *                                                         273 002
           05  B076-LIMOPER            PIC  9(007)V99  COMP-3.
      *                                                         275 005
           05  B076-RAMO               PIC  9(002).
      *                                                         280 002
      *                                                         282 007
           05  B076-TELEFONE           PIC  9(007)     COMP-3.
      *                                                         289 004
      *    TAXA MAQ
      *    SE '*' = ISENTO TAXA
      *    SE ' ' = COBRA  TAXA
      *
           05  B076-TAXAMAQ            PIC  X(001).
      *                                                         293 001
      *    HOT-CARD
      *    SE 1 = RECEBE HOT-CARD
      *    SE 2 = NAO RECEBE HOT-CARD
      *
           05  B076-HOTCARD            PIC  9(001).
      *                                                         294 001
           05  B076-SUFCEPVE           PIC  X(003).
      *                                                         295 003
           05  B076-NUMFILIA           PIC  9(003)     COMP-3.
      *                                                         298 002
           05  B076-DTADESAO           PIC  9(007)     COMP-3.
           05  B076-QTDEMAQ            PIC  9(005)     COMP-3.
      *                                                         304 003
           05  B076-CODIATV            PIC  9(005)     COMP-3.
      *                                                         307 003
           05  B076-NOMEFANT           PIC  X(040).
      *                                                         310 040
           05  B076-ACERKELO           PIC  9(001).
      *                                                         350 001

