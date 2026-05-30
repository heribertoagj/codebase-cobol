      *---------------------------------------------------------------*
      * BOOK DO ARQUIVO MOVIMENTO DE ORPAG                            *
      * TAMANHO : 0200   (MOVTO.EMPRESA/POS/DIGITADO)                 *
      * I#BVVE84     (COPIA DO I#BVCO34)  ULTIMA ALTERACAO : 28/06/95 *
      *---------------------------------------------------------------*
      *                                                         POS TAM

      *----------- TIPO DE REGISTRO 1 - REMESSA -----------------------*

       01      B084-REG-REMESSA-1.
      *                                                         001 200
           05  B084-TIPO-REGISTRO-1    PIC 9(01).
      *                                                         001 001
           05  B084-MAPA-1.
      *                                                         002 007
               10  B084-NUM-MAPA-1     PIC 9(06).
      *                                                         002 006
               10  B084-CPL-MAPA-1     PIC 9(01).
      *                                                         008 001
           05  B084-REMESSA-1.
      *                                                         009 010
               10  B084-NUM-REMESSA-1  PIC 9(07).
      *                                                         009 007
               10  B084-CPL-REMESSA-1  PIC 9(03).
      *                                                         016 003
           05  B084-ESTABELECIMENTO-1  PIC 9(13).
           05  FILLER  REDEFINES B084-ESTABELECIMENTO-1.
      *                                                         019 013
               10  B084-PREF-EST-1     PIC 9(04).
      *                                                         019 004
               10  B084-BCO-EST-1      PIC 9(03).
      *                                                         023 003
               10  B084-NUM-EST-1      PIC 9(06).
      *                                                         026 006
           05  B084-TIPO-CAPT-1        PIC 9(02).
      *                                                         032 002
           05  B084-VR-REMESSA-1       PIC 9(13)V9(02).
      *                                                         034 015
           05  B084-TOT-PARCELAS-1     PIC 9(05).
      *                                                         049 005
           05  B084-QTDE-ORPAG-1       PIC 9(05).
      *                                                         054 005
           05  B084-SUBCENTRO-1.
      *                                                         059 005
               10  B084-CODIGO-1       PIC 9(04).
      *                                                         059 004
               10  B084-DIGITO-1       PIC 9(01).
      *                                                         063 001
           05  B084-DIA-1              PIC 9(02).
      *                                                         064 002
           05  B084-DT-RECEBIMENTO-1   PIC 9(08).
           05  FILLER REDEFINES B084-DT-RECEBIMENTO-1.
               10  B084-AA1-RECEB-1    PIC 99.
               10  B084-AA2-RECEB-1    PIC 99.
               10  B084-MM-RECEB-1     PIC 99.
               10  B084-DD-RECEB-1     PIC 99.
      *                                                         066 008
           05  B084-SIT-REMESSA-1      PIC 9(01).
      *                                                         074 001
           05  B084-TIPO-PAGAMENTO-1   PIC 9(01).
      *                                                         075 001
           05  B084-DT-MOVIMENTO-1     PIC 9(08).
      *                                                         076 008
           05  B084-VR-ORIG-REMESSA-1  PIC 9(13)V99.
      *                                                         084 015
           05  B084-COD-REJEICAO-1     PIC 9(03).
      *                                                         099 003
           05  B084-DATA-REJEICAO-1    PIC 9(08).
      *                                                         102 008
           05  FILLER                  PIC X(07).
      *                                                         110 007
           05  B084-CHAVE-IMAGEM-1     PIC X(15).
      *                                                         117 015
           05  FILLER  REDEFINES B084-CHAVE-IMAGEM-1.
      *
      *  DATA NO FORMATA AADDD
      *
               10 B084-DTMOVTO-1       PIC 9(005).
      *                                                         117 005
               10 B084-TPCAPT-1        PIC 9(002).
      *                                                         122 002
               10 B084-NUMSEQ-1        PIC 9(008).
      *                                                         124 008
           05  B084-VALORIG-1          PIC 9(013)V99.
      *                                                         132 015
           05  B084-COD-LAYOUT-1       PIC X(003).
      *                                                         147 003
      *  001 - POS
      *  002 - VARIG
      *
           05  B084-LAYOUT-VARIAVEL-1  PIC X(047).
      *                                                         150 051
           05  B084-LAYOUT-001-1  REDEFINES B084-LAYOUT-VARIAVEL-1.
      *                                                         150 051
               10  B084-LOTE-ORG-POS-2 PIC 9(007).
      *                                                         150 007
               10  FILLER              PIC X(040).
      *                                                         157 039
           05  FILLER                  PIC X(004).
      *                                                         197 004

      *------------ TIPO DE REGISTRO 2 - ORPAG -----------------------*

       01      B084-REG-ORPAG-2  REDEFINES  B084-REG-REMESSA-1.
      *                                                         001 200
           05  B084-TIPO-REGISTRO-2    PIC 9(01).
      *                                                         001 001
           05  B084-MAPA-2.
      *                                                         002 007
               10  B084-NUM-MAPA-2     PIC 9(06).
      *                                                         002 006
               10  B084-CPL-MAPA-2     PIC 9(01).
      *                                                         008 001
           05  B084-REMESSA-2.
      *                                                         009 010
               10  B084-NUM-REMESSA-2  PIC 9(07).
      *                                                         009 007
               10  B084-CPL-REMESSA-2  PIC 9(03).
      *                                                         016 003
           05  B084-ESTABELECIMENTO-2  PIC 9(13).
           05  FILLER REDEFINES B084-ESTABELECIMENTO-2.
      *                                                         019 013
               10  B084-PREF-EST-2     PIC 9(04).
      *                                                         019 004
               10  B084-BCO-EST-2      PIC 9(03).
      *                                                         023 003
               10  B084-NUM-EST-2      PIC 9(06).
      *                                                         026 006
           05  B084-CARTAO-PORTADOR-2.
      *                                                         032 016
               10  B084-BIN-CARTAO-2   PIC 9(06).
      *                                                         032 006
               10  B084-NUM-CARTAO-2   PIC 9(10).
      *                                                         038 010
           05  B084-SUFI-CARTAO-2      PIC 9(03).
      *                                                         048 003
           05  B084-DT-COMPRA-2        PIC 9(08).
           05  FILLER REDEFINES B084-DT-COMPRA-2.
               10  B084-AA1-COMPRA-2   PIC 99.
               10  B084-AA2-COMPRA-2   PIC 99.
               10  B084-MM-COMPRA-2    PIC 99.
               10  B084-DD-COMPRA-2    PIC 99.
      *                                                         051 008
           05  B084-NUM-ORPAG-2        PIC 9(07).
      *                                                         059 007
           05  B084-VR-ORPAG-2         PIC 9(11)V9(02).
      *                                                         066 013
           05  B084-VR-OUTROS-SERV-2   PIC 9(11)V9(02).
      *                                                         079 013
           05  B084-QTDE-PARC-2        PIC 9(02).
      *                                                         092 002
           05  B084-COD-AUTOR-2        PIC 9(06).
      *                                                         094 006
           05  B084-SIT-ORPAG-2        PIC 9(01).
      *                                                         100 001
           05  B084-COD-REJEICAO-2     PIC 9(03).
      *                                                         101 003
           05  B084-DATA-REJEICAO-2    PIC 9(08).
      *                                                         104 008
           05  B084-COD-DESDOBR-2      PIC 9(04).
      *                                                         112 004
           05  B084-IND-TRANS-ELETR-2  PIC X(01).
      *                                                         116 001
           05  B084-CHAVE-IMAGEM-2     PIC X(15).
      *                                                         117 015
           05  FILLER  REDEFINES B084-CHAVE-IMAGEM-2.
      *
      *  DATA NO FORMATA AADDD
      *
               10 B084-DTMOVTO-2       PIC 9(005).
      *                                                         117 005
               10 B084-TPCAPT-2        PIC 9(002).
      *                                                         122 002
               10 B084-NUMSEQ-2        PIC 9(008).
      *                                                         124 008
           05  B084-CODMOEDA-2         PIC X(001).
      *                                                         132 001
           05  FILLER                  PIC X(003).
      *                                                         133 003
           05  B084-SUBCTRO-2          PIC 9(004).
      *                                                         136 004
           05  B084-TAXA-SAQUE-2       PIC 9(007)V9(04) COMP-3.
      *                                                         140 006
           05  B084-BONUS-2            PIC 9(003)V99.
      *                                                         146 005
           05  B084-COD-LAYOUT-2       PIC X(003).
      *                                                         151 003
      *    001 - POS
      *    002 - VARIG
      *
           05  B084-LAYOUT-VARIAVEL-2   PIC X(047).
      *                                                         154 047
           05  B084-LAYOUT-001-2  REDEFINES  B084-LAYOUT-VARIAVEL-2.
      *                                                         154 047
               10  B084-COD-PARCEL-001-2 PIC X(001).
      *                                                         154 001
               10  FILLER                PIC X(046).
      *                                                         155 046
           05  B084-LAYOUT-002-2  REDEFINES  B084-LAYOUT-VARIAVEL-2.
      *                                                         154 047
               10  B084-CPR-VENDAS-002-2 PIC X(015).
      *                                                         154 015
               10  B084-OBR-FINANC-002-2 PIC X(007).
      *                                                         169 007
               10  B084-IDENT-ADM-002-2  PIC X(005).
      *                                                         176 005
               10  B084-VL-PRI-PRE-002-2 PIC 9(009)V99 COMP-3.
      *                                                         181 006
               10  B084-VL-DEM-PAR-002-2 PIC 9(009)V99 COMP-3.
      *                                                         187 006
               10  FILLER                PIC X(008).
      *                                                         193 008

       01      B084-REG-TIPO-3   REDEFINES  B084-REG-REMESSA-1.
      *
           05  FILLER                  PIC X(200).
      *                                                         001 200
