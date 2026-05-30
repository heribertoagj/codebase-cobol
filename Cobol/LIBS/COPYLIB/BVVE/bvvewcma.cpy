      *----------------------------------------------------------------*
      * ELEMENTO : BVVE - CARTAO DE CREDITO - VENDEDOR                 *
      *----------------------------------------------------------------*
      * BOOK     : BVVEWCMA                                            *
      * DESCRICAO: COMOM AREA PADRAO DE NAVEGACAO INCLUSAO DE PROPOSTA *
      * EMPRESA  : FOURSYS                                             *
      * AUTOR    : JULIANA TURCO                                       *
      * TAMANHO  : 1110 BYTES                                          *
      *----------------------------------------------------------------*
      * DATA       | AUTOR           | DESCRICAO / MANUTENCAO          *
      *----------------------------------------------------------------*
      *            |                 |                                 *
      *----------------------------------------------------------------*
       01  WRK-COMMAREA.
           03  WRK-COMM-PADRAO.
               05  WRK-COMM-TRANSACAO    PIC  X(04)        VALUE SPACES.
               05  WRK-COMM-FASE         PIC  X(01)        VALUE SPACES.
               05  WRK-COMM-PAG          PIC  9(05) COMP-3 VALUE ZEROS.
               05  WRK-COMM-ULT-PAG      PIC  9(05) COMP-3 VALUE ZEROS.
               05  WRK-COMM-MENSAG       PIC  X(79)        VALUE SPACES.
               05  FILLER                PIC  X(20)        VALUE SPACES.
      *----------------------------------------------------------------*
           03  WRK-COMM-PGM-2950.
               05  WRK-COMM-AG-ORIG      PIC  9(04)        VALUE ZEROS.
               05  WRK-COMM-OPCAO        PIC  X(01)        VALUE SPACES.
               05  WRK-COMM-CNPJ         PIC  9(08)        VALUE ZEROS.
               05  WRK-COMM-FILIAL       PIC  9(04)        VALUE ZEROS.
               05  WRK-COMM-CONTROL      PIC  9(02)        VALUE ZEROS.
               05  FILLER                PIC  X(20)        VALUE SPACES.
      *----------------------------------------------------------------*
           03  WRK-COMM-PGM-1802.
               05  WRK-COMM-PROD         PIC  9(03)        VALUE ZEROS.
               05  WRK-COMM-DESCR-PROD   PIC  X(30)        VALUE SPACES.
               05  FILLER                PIC  X(20)        VALUE SPACES.
      *----------------------------------------------------------------*
           03  WRK-COMM-PGM-1803.
               05  WRK-COMM-TIPOEMP      PIC  9(02)        VALUE ZEROS.
               05  WRK-COMM-DESCR-EMP    PIC  X(30)        VALUE SPACES.
               05  FILLER                PIC  X(20)        VALUE SPACES.
      *----------------------------------------------------------------*
           03  WRK-COMM-PGM-1807.
               05  WRK-COMM-FASE-1807    PIC  X(01)        VALUE SPACES.
               05  WRK-COMM-FORMA-PG     PIC  X(01)        VALUE SPACES.
               05  WRK-COMM-PRAZO-PG     PIC  9(03)        VALUE ZEROS.
               05  WRK-COMM-PRAZO-PG-X  REDEFINES WRK-COMM-PRAZO-PG
                                         PIC  X(03).
               05  WRK-COMM-MEIO-PG-X.
                   07  WRK-COMM-MEIO-PG  PIC  9(03)        VALUE ZEROS.
               05  WRK-COMM-DESCR-MEIO   PIC  X(20)        VALUE SPACES.
               05  WRK-COMM-AGENCIA-X.
                   07  WRK-COMM-AGENCIA  PIC  9(05)        VALUE ZEROS.
               05  WRK-COMM-CONTA        PIC  9(07)        VALUE ZEROS.
               05  WRK-COMM-DIG-CONTA    PIC  X(01)        VALUE SPACES.
               05  WRK-COMM-CONTA-GAR    PIC  X(01)        VALUE SPACES.
               05  WRK-COMM-VCONTR-X.
                   07  WRK-COMM-VCONTR   PIC  9(13)        VALUE ZEROS.
               05  WRK-COMM-DINCL-CONTR-X.
                   07  WRK-COMM-DINCL-CONTR
                                         PIC  9(08)        VALUE ZEROS.
               05  WRK-COMM-DVCTO-LIM-ROTVO-X.
                   07  WRK-COMM-DVCTO-LIM-ROTVO
                                         PIC  9(08)        VALUE ZEROS.
               05  WRK-COMM-COD-AUT-CONTA-X.
                   07  WRK-COMM-COD-AUT-CONTA
                                             PIC  9(10)    VALUE ZEROS.
               05  WRK-COMM-PAG-M            PIC  9(05) COMP-3 VALUE 0.
               05  WRK-COMM-ULT-PAG-M        PIC  9(05) COMP-3 VALUE 0.
               05  WRK-COMM-NCONTAGAR.
                   07  WRK-COMM-CONTAGAR     PIC  X(01)    VALUE SPACES.
                   07  WRK-COMM-LIMIT-CTAG   PIC  9(13)    VALUE ZEROS.
                   07  WRK-COMM-VALDD-CTAG   PIC  X(10)    VALUE SPACES.
               05  WRK-COMM-NCARTAO.
                   07  WRK-COMM-CARTAO       PIC  X(01)    VALUE SPACES.
                   07  WRK-COMM-NCARTA1      PIC  X(04)    VALUE SPACES.
                   07  WRK-COMM-NCARTA2      PIC  X(04)    VALUE SPACES.
                   07  WRK-COMM-NCARTA3      PIC  X(04)    VALUE SPACES.
                   07  WRK-COMM-NCARTA4      PIC  X(04)    VALUE SPACES.
                   07  WRK-COMM-LIMIT-CARTAO PIC  9(13)    VALUE ZEROS.
                   07  WRK-COMM-VALDD-CARTAO PIC  X(10)    VALUE SPACES.
               05  WRK-COMM-NCONE.
                   07  WRK-COMM-OUTROS       PIC  X(01)    VALUE SPACES.
                   07  WRK-COMM-NCON1        PIC  X(04)    VALUE SPACES.
                   07  WRK-COMM-NCON2        PIC  X(05)    VALUE SPACES.
                   07  WRK-COMM-NCON3        PIC  X(12)    VALUE SPACES.
                   07  WRK-COMM-LIMIT-CONE   PIC  9(13)    VALUE ZEROS.
                   07  WRK-COMM-VALDD-CONE   PIC  X(10)    VALUE SPACES.
                   07  WRK-COMM-CART-CONE    PIC  X(03)    VALUE SPACES.
                05  FILLER                   PIC  X(20)    VALUE SPACES.
      *----------------------------------------------------------------*
           03  WRK-COMM-PGM-1804.
               05  WRK-COMM-QTD-PAG-1804 PIC  9(05)        VALUE ZEROS.
               05  WRK-COMM-QTD-BEN      PIC  9(06)        VALUE ZEROS.
               05  WRK-COMM-QTD-BEN-X REDEFINES WRK-COMM-QTD-BEN
                                         PIC  X(06).
               05  WRK-COMM-VL-MEDIO     PIC  9(09)V99     VALUE ZEROS.
               05  FILLER                PIC  X(20)        VALUE SPACES.
      *----------------------------------------------------------------*
           03  WRK-COMM-PGM-1805.
               05  WRK-COMM-QTD-PAG-1805 PIC  9(05)        VALUE ZEROS.
               05  WRK-COMM-COD-AUT-X.
                   07  WRK-COMM-COD-AUT  PIC  9(10)        VALUE ZEROS.
               05  WRK-COMM-VALOR        PIC  9(03)V99     VALUE ZEROS.
               05  WRK-COMM-TARIF-FORMT  PIC  X(01)        VALUE SPACES.
               05  FILLER                PIC  X(20)        VALUE SPACES.
      *----------------------------------------------------------------*
           03  WRK-COMM-PGM-1808.
               05  WRK-COMM-NOME-EMP     PIC  X(40)        VALUE SPACES.
               05  WRK-COMM-NOME-FAN     PIC  X(20)        VALUE SPACES.
               05  WRK-COMM-QTD-FUNC     PIC  9(06)        VALUE ZEROS.
               05  WRK-COMM-NOME-FIL     PIC  X(35)        VALUE SPACES.
               05  WRK-COMM-CEPG.
                 07  WRK-COMM-CEP        PIC  9(05)        VALUE ZEROS.
                 07  WRK-COMM-COMPL-CEP  PIC  9(03)        VALUE ZEROS.
               05  WRK-COMM-CEP-R REDEFINES WRK-COMM-CEPG
                                         PIC  9(08).
               05  WRK-COMM-DDD-X.
                   07  WRK-COMM-DDD      PIC  9(02)        VALUE ZEROS.
               05  WRK-COMM-TELEFONE-X.
                   07  WRK-COMM-TELEFONE PIC  9(09)        VALUE ZEROS.
               05  WRK-COMM-ENDERECO     PIC  X(50)        VALUE SPACES.
               05  WRK-COMM-NUMERO       PIC  X(05)        VALUE SPACES.
               05  WRK-COMM-COMPL-ENDERECO
                                         PIC  X(25)        VALUE SPACES.
               05  WRK-COMM-CIDADE       PIC  X(28)        VALUE SPACES.
               05  WRK-COMM-UF           PIC  X(02)        VALUE SPACES.
               05  WRK-COMM-REEMIS       PIC  X(01)        VALUE SPACES.
               05  WRK-COMM-LIMITE-TELA  PIC  9(13)        VALUE ZEROS.
               05  FILLER                PIC  X(20)        VALUE SPACES.
      *----------------------------------------------------------------*
           03  WRK-COMM-PGM-1809.
               05  WRK-COMM-NOME-GRAVACAO
                                         PIC  X(19)        VALUE SPACES.
               05  WRK-COMM-ENTREGA-CENTR
                                         PIC  X(01)        VALUE SPACES.
               05  WRK-COMM-FILIALE      PIC  9(04)        VALUE ZEROS.
               05  WRK-COMM-CONTROLE     PIC  9(02)        VALUE ZEROS.
               05  WRK-COMM-FILIALF      PIC  9(04)        VALUE ZEROS.
               05  WRK-COMM-CONTROLF     PIC  9(02)        VALUE ZEROS.
               05  WRK-COMM-FATURA-CENTR PIC  X(01)        VALUE SPACES.
               05  WRK-COMM-FILIALC      PIC  9(04)        VALUE ZEROS.
               05  WRK-COMM-CONTROLC     PIC  9(02)        VALUE ZEROS.
               05  WRK-COMM-FUNCD        PIC  X(01)        VALUE SPACES.
               05  WRK-COMM-CPFD         PIC  9(09)        VALUE ZEROS.
               05  WRK-COMM-CONTROLD     PIC  9(02)        VALUE ZEROS.
               05  WRK-COMM-DTNASCD      PIC  9(08)        VALUE ZEROS.
               05  WRK-COMM-NOMED        PIC  X(35)        VALUE SPACES.
               05  WRK-COMM-EMAIL        PIC  X(40)        VALUE SPACES.
               05  WRK-COMM-DDDD-X.
                   07  WRK-COMM-DDDD     PIC  9(02)        VALUE ZEROS.
               05  WRK-COMM-TELD-X.
                   07  WRK-COMM-TELD     PIC  9(09)        VALUE ZEROS.
               05  WRK-COMM-MESMOS       PIC  X(01)        VALUE SPACES.
               05  WRK-COMM-FUNCO        PIC  X(01)        VALUE SPACES.
               05  WRK-COMM-CPFO         PIC  9(09)        VALUE ZEROS.
               05  WRK-COMM-CONTROLO     PIC  9(02)        VALUE ZEROS.
               05  WRK-COMM-DTNASCO      PIC  9(08)        VALUE ZEROS.
               05  WRK-COMM-NOMEO        PIC  X(35)        VALUE SPACES.
               05  WRK-COMM-EMAIL2       PIC  X(40)        VALUE SPACES.
               05  WRK-COMM-DDDO-X.
                   07  WRK-COMM-DDDO     PIC  9(02)        VALUE ZEROS.
               05  WRK-COMM-TELO-X.
                   07  WRK-COMM-TELO     PIC  9(09)        VALUE ZEROS.
               05  WRK-COMM-CPF3         PIC  9(09)        VALUE ZEROS.
               05  WRK-COMM-CONTROL3     PIC  9(02)        VALUE ZEROS.
               05  WRK-COMM-CODFUNC      PIC  9(07)        VALUE ZEROS.
               05  FILLER                PIC  X(20)        VALUE SPACES.
      *----------------------------------------------------------------*
