      *================================================================*
      *================================================================*
      *=         ESTA INC ESTA EM CONFORMIDADE COM O ANO 2000.        =*
      *=         DATA:  03/05/1999                                    =*
      *=         CPM   -   KIT2000                                    =*
      *================================================================*
      *================================================================*
      *---------------------------------------------------------------*
      * BOOK DO ARQUIVO MOVIMENTO DE ORPAG FORMATADO                  *
      * TAMANHO : 0354                                                *
      * I#BVVE74                          ULTIMA ALTERACAO : 07/06/95 *
      *---------------------------------------------------------------*
      *                                                               *
      * SE HOUVER ALTERACAO NAS PRIMEIRAS 300 POSICOES DESTE BOOK,    *
      * ALTERAR TAMBEM O BOOK I#BVCO91.                               *
      *                                                               *
      *---------------------------------------------------------------*

      *----------- TIPO DE REGISTRO 1 - REMESSA -----------------------*

       01      B074-REG-REMESSA-1.
      *                                                         001 354
         03    B074-TIPO-REGISTRO-1    PIC 9(01).
      *                                                         001 001
         03    B074-NUMLOTE-1.
      *                                                         002 017
           05  B074-SUB-CTRO-1         PIC 9(05).
           05  FILLER  REDEFINES  B074-SUB-CTRO-1.
      *                                                         002 005
               10  B074-SUBCTRO-1      PIC 9(04).
      *                                                         002 004
               10  B074-DIGSBC-1       PIC 9(01).
      *                                                         006 001
           05  B074-DIA-1              PIC 9(02).
      *                                                         007 002
           05    B074-REMESSA-1.
      *                                                         009 010
             10  B074-NUM-REMESSA-1    PIC 9(07).
      *                                                         009 007
      *                                                         016 003
         03    FILLER  REDEFINES  B074-NUMLOTE-1.
      *                                                         002 017
           05  B074-LOTE-1             PIC X(14).
      *                                                         002 014
           05  FILLER                  PIC X(03).
      *                                                         016 003
         03    B074-TIPO-PAGAMENTO-1   PIC 9(01).
      *                                                         019 001
         03    B074-MAPA-1             PIC 9(07).
         03    FILLER  REDEFINES  B074-MAPA-1.
      *                                                         020 007
           05  B074-NUM-MAPA-1         PIC 9(06).
      *                                                         020 006
           05  B074-CPL-MAPA-1         PIC 9(01).
      *                                                         026 001
         03    B074-VENDEDOR-1.
      *                                                         027 013
           05  B074-PREF-VEN-1         PIC 9(04).
      *                                                         027 004
           05  FILLER  REDEFINES  B074-NUMEVEND-1.
      *                                                         031 009
               10  B074-BCO-VEN-1      PIC 9(03).
      *                                                         031 003
               10  B074-NUMVEN-1       PIC 9(06).
      *                                                         034 006
               10  FILLER  REDEFINES  B074-NUMVEN-1.
      *
                   15 B074-NUM-VEN-1   PIC 9(05).
      *                                                         034 005
                   15 B074-DIG-VEN-1   PIC 9(01).
      *                                                         039 001
         03    B074-TIPO-CAPT-1        PIC 9(02).
      *                                                         040 002
         03    B074-VR-REMESSA-1       PIC 9(13)V9(02).
      *                                                         042 015
         03    B074-TOT-PARCELAS-1     PIC 9(07)   COMP-3.
      *                                                         057 004
         03    B074-QTDE-ORPAG-1       PIC 9(05)   COMP-3.
      *                                                         061 003
      *                                                         064 008
         03    FILLER  REDEFINES  B074-DT-RECEBIMENTO-1.
           05  B074-AA1-RECEB          PIC 99.
      *                                                         064 002
           05  B074-AA2-RECEB          PIC 99.
      *                                                         066 002
           05  B074-MES-RECEB          PIC 99.
      *                                                         068 002
           05  B074-DIA-RECEB          PIC 99.
      *                                                         070 002
         03    B074-SIT-REMESSA-1      PIC 9(01).
      *                                                         072 001
         03  B074-NOMEVEND-1           PIC X(035).
      *                                                         073 035
         03  B074-BANCO-1              PIC 9(003)  COMP-3.
      *                                                         108 002
         03  B074-AGENCIA-1            PIC 9(005)  COMP-3.
      *                                                         110 003
         03  B074-RAZAO-1              PIC 9(005)  COMP-3.
      *                                                         113 003
      *                                                         116 004
         03  B074-NUMDIAS-1            PIC 9(002).
      *                                                         120 002
         03  B074-TIPCRED-1            PIC X(001).
      *                                                         122 001
         03  B074-SUBCTRO-ANT-1        PIC 9(004).
      *                                                         123 004
         03  B074-CODATIV-1            PIC 9(004).
      *                                                         127 004
         03  B074-VALORIG-1            PIC 9(013)V9(02).
      *                                                         131 015
      *
CPMCPM*        DATA CRIACAO DO MAPA  (SSAADDD)
      *
CPMCPM   03  B074-DTCRIMAP-1           PIC 9(007).
      *                                                         146 007
         03  B074-MOEDA-FINAN-1.
      *                                                         153 002
             05  B074-CODMOEDA-1       PIC X(001).
      *                                                         153 001
      *                                                         154 001
         03  B074-NUMREFER-1           PIC X(023).
      *                                                         155 023
         03  FILLER  REDEFINES B074-NUMREFER-1.
          05 B074-FIX7AC1-1            PIC X(001).
      *                                                         155 001
          05 B074-BINVAC1-1            PIC 9(006).
      *                                                         156 006
          05 B074-DTCPAC1-1            PIC 9(004).
      *                                                         162 004
          05 B074-NVISAC1-1            PIC 9(011).
      *                                                         166 011
          05 B074-DIGVIC1-1            PIC 9(001).
      *                                                         177 001
         03  B074-IND-SUBCTRO-ANT-1    PIC 9(003)  COMP-3.
      *                                                         178 002
         03  B074-TAXA-COMIS-VEND-1    PIC 9(003)V99 COMP-3.
      *                                                         180 003
         03  FILLER                    PIC X(105).
      *                                                         183 105
      *                                                         288 017
         03  FILLER  REDEFINES B074-CHAVE-IMAGEM-1.
      *
CPMCPM*  DATA NO FORMATO SSAADDD
      *
CPMCPM       05 B074-DTMOVTO-1         PIC 9(007).
      *                                                         288 007
             05 B074-TPCAPT-1          PIC 9(002).
      *                                                         295 002
             05 B074-NUMSEQ-1          PIC 9(008).
      *                                                         297 008
         03  B074-COD-LAYOUT-1         PIC X(003).
      *                                                         305 003
      *   001 - POS
      *   002 - VARIG
      *
         03  B074-LAYOUT-VARIAVEL-1    PIC X(047).
      *                                                         308 047
         03  B074-LAYOUT-001-1   REDEFINES B074-LAYOUT-VARIAVEL-1.
      *                                                         308 047
      *                                                         308 007
             05 FILLER                 PIC X(040).
      *                                                         315 040

      *------------ TIPO DE REGISTRO 2 - ORPAG -----------------------*

       01      B074-REG-ORPAG-2  REDEFINES  B074-REG-REMESSA-1.
         03    B074-TIPO-REGISTRO-2    PIC 9(01).
      *                                                         001 001
         03    B074-NUMLOTE-2.
      *                                                         002 005
           05  B074-SUB-CTRO-2         PIC 9(05).
           05  FILLER  REDEFINES  B074-SUB-CTRO-2.
      *                                                         002 005
               10  B074-SUBCTRO-2      PIC 9(04).
      *                                                         002 004
               10  B074-DIGSBC-2       PIC 9(01).
      *                                                         006 001
           05  B074-DIA-2              PIC 9(02).
      *                                                         007 002
      *                                                         009 010
             10  B074-NUM-REMESSA-2    PIC 9(07).
      *                                                         009 007
             10  B074-CPL-REMESSA-2    PIC 9(03).
      *                                                         016 003
         03    FILLER  REDEFINES  B074-NUMLOTE-2.
      *                                                         002 017
           05  B074-LOTE-2             PIC X(14).
      *                                                         002 014
           05  FILLER                  PIC X(03).
      *                                                         016 003
         03    B074-TIPO-PAGAMENTO-2   PIC 9(01).
      *                                                         019 001
         03    B074-MAPA-2             PIC 9(07).
         03    FILLER  REDEFINES  B074-MAPA-2.
      *                                                         020 007
           05  B074-NUM-MAPA-2         PIC 9(06).
      *                                                         020 006
           05  B074-CPL-MAPA-2         PIC 9(01).
      *                                                         026 001
      *                                                         027 013
           05  B074-PREF-VEN-2         PIC 9(04).
      *                                                         027 004
           05  B074-NUMEVEND-2         PIC 9(09).
           05  FILLER  REDEFINES  B074-NUMEVEND-2.
      *                                                         031 009
               10  B074-BCO-VEN-2      PIC 9(03).
      *                                                         031 003
               10  B074-NUMVEN-2       PIC 9(06).
      *                                                         034 006
               10  FILLER  REDEFINES  B074-NUMVEN-2.
      *
                   15 B074-NUM-VEN-2   PIC 9(05).
      *                                                         034 005
                   15 B074-DIG-VEN-2   PIC 9(01).
      *                                                         039 001
         03    B074-TIPO-CAPT-2        PIC 9(02).
      *                                                         040 002
         03    B074-CARTAO-PORTADOR-2.
      *                                                         042 016
      *                                                         042 006
           05  B074-NUM-CARTAO-2       PIC 9(10).
      *                                                         048 010
         03    B074-SUFI-CARTAO-2      PIC 9(03).
      *                                                         058 003
         03    B074-DT-COMPRA-2        PIC 9(08).
      *                                                         061 008
         03    FILLER  REDEFINES  B074-DT-COMPRA-2.
           05  B074-AA1-COMPRA-2       PIC 9(02).
      *                                                         061 002
           05  B074-AA2-COMPRA-2       PIC 9(02).
      *                                                         063 002
           05  B074-MES-COMPRA-2       PIC 9(02).
      *                                                         065 002
           05  B074-DIA-COMPRA-2       PIC 9(02).
      *                                                         067 002
         03    B074-NUM-ORPAG-2        PIC 9(07).
      *                                                         069 007
         03    B074-VR-ORPAG-2         PIC 9(11)V9(02).
      *                                                         076 013
      *                                                         089 013
         03    B074-QTDE-PARC-2        PIC 9(02).
      *                                                         102 002
         03    B074-COD-AUTOR-2        PIC 9(06).
      *                                                         104 006
         03    B074-SIT-ORPAG-2        PIC 9(01).
      *                                                         110 001
      *
      *        SITUACAO DA ORPAG
      *        0 - CAPTADA
      *        1 - RETIDA
      *        2 - REJEITADA
      *        3 - LIBER. PELA CONSIST.
      *        4 - RETIDA NO MODULO III
      *        5 - REJEITADA NO MODULO III
      *        6 - LIBERADA PELO MODULO III
      *
         03    B074-COD-REJEICAO-2     PIC 9(03).
      *                                                         111 003
         03    B074-DATA-REJEICAO-2    PIC 9(08).
         03    FILLER  REDEFINES  B074-DATA-REJEICAO-2.
           05  B074-AA1-REJEICAO-2     PIC 9(02).
      *                                                         114 002
           05  B074-AA2-REJEICAO-2     PIC 9(02).
      *                                                         116 002
           05  B074-MES-REJEICAO-2     PIC 9(02).
      *                                                         118 002
           05  B074-DIA-REJEICAO-2     PIC 9(02).
      *                                                         120 002
         03    B074-COD-DESDOBR-2      PIC 9(04).
      *                                                         122 004
         03    B074-IND-TRANS-ELETR-2  PIC X(01).
      *                                                         126 001
      *
      *        INDICADOR DE TRANSACAO ELETRONICA
      *
      *        0 - DIGITADA
      *        1 - ELETRONICA
      *---------------------------------------------------------------*
      *   A PARTIR DO JOB BACC2305 - PGM  BACC1178, OS REGISTROS  COM
      *   CONFORME ABAIXO, RETORNANDO A INFORMACAO ORIGIMAL DO ARQ. :
      *
      *       "BRANCO" - DIGITADA
      *       "*"      - ELETRONICA / PS-2000
      *---------------------------------------------------------------*
      *
         03    B074-CARTAO-PORT-ANT-2.
      *                                                         127 016
           05  B074-BIN-ANT-2          PIC 9(06).
      *                                                         127 006
           05  B074-NUMCART-ANT-2      PIC 9(10).
      *                                                         133 010
         03  B074-BANCO-VEND-2         PIC 9(002).
      *                                                         143 002
         03  B074-BANCO-COMP-2         PIC 9(002).
      *                                                         145 002
         03  B074-TIPCRED-2            PIC X(001).
      *                                                         147 001
         03  B074-SUBCTRO-ANT-2        PIC 9(004).
      *                                                         148 004
CPMCPM*        DATA CRIACAO DO MAPA  (SSAADDD)
      *
CPMCPM   03  B074-DTCRIMAP-2           PIC 9(007).
      *                                                         152 007
         03  B074-DIADEB-2             PIC 9(002).
      *                                                         159 002
         03  B074-MOEDA-FINAN-2.
      *                                                         161 002
             05  B074-CODMOEDA-2       PIC X(001).
      *                                                         161 001
             05  B074-CODFINAN-2       PIC X(001).
      *                                                         162 001
         03  B074-NUMREFER-2           PIC X(023).
      *                                                         163 023
         03  FILLER  REDEFINES B074-NUMREFER-2.
          05 B074-FIX7AC2-2            PIC X(001).
      *                                                         163 001
          05 B074-BINVAC2-2            PIC 9(006).
      *                                                         164 006
          05 B074-DTCPAC2-2            PIC 9(004).
          05 B074-NVISAC2-2            PIC 9(011).
      *                                                         174 011
          05 B074-DIGVIC2-2            PIC 9(001).
      *                                                         185 001
         03  B074-PERC-BONUS-2         PIC 9(003)V99.
      *                                                         186 005
         03  B074-VALOR-PRIPREST-2     PIC 9(009)V99  COMP-3.
      *                                                         191 006
         03  B074-VALOR-DEMPREST-2     PIC 9(009)V99  COMP-3.
      *                                                         197 006
         03  B074-PARCELADO-2          PIC 9(001).
      *                                                         203 001
         03  B074-NOME-PORTADOR-2      PIC X(035).
      *                                                         204 035
         03  B074-CODATIV-2            PIC 9(004).
      *                                                         239 004
         03  B074-TAXA-SAQUE-2         PIC 9(007)V9(04)  COMP-3.
      *                                                         243 006
         03  B074-N-GERA-CPR-2         PIC X(001).
      *                                                         249 001
      *                                                         250 011
             05 B074-PREFCPR-2         PIC 9(005)  COMP-3.
      *                                                         250 003
             05 B074-BCOCPR-2          PIC 9(005)  COMP-3.
      *                                                         253 003
             05 B074-TIPOCPR-2         PIC 9(001).
      *                                                         256 001
             05 B074-NUMECPR-2         PIC 9(007)  COMP-3.
      *                                                         257 004
         03  B074-NOMVEND-INC-2        PIC X(025).
      *                                                         261 025
         03  B074-IND-SUBCTRO-ANT-2    PIC 9(003)  COMP-3.
      *                                                         286 002
CPMCPM   03  B074-CHAVE-IMAGEM-2       PIC X(017).
      *                                                         288 017
         03  FILLER  REDEFINES B074-CHAVE-IMAGEM-2.
      *
CPMCPM*  DATA NO FORMATO SSAADDD
      *
CPMCPM       05 B074-DTMOVTO-2         PIC 9(007).
             05 B074-TPCAPT-2          PIC 9(002).
      *                                                         295 002
             05 B074-NUMSEQ-2          PIC 9(008).
      *                                                         297 008
         03  B074-COD-LAYOUT-2         PIC X(003).
      *                                                         305 003
      *   001 - POS
      *   002 - VARIG
      *
         03  B074-LAYOUT-VARIAVEL-2    PIC X(047).
      *                                                         308 047
         03  B074-LAYOUT-001-2  REDEFINES B074-LAYOUT-VARIAVEL-2.
      *                                                         308 047
             05 B074-COD-PARCEL-001-2  PIC X(001).
      *                                                         308 001
             05 FILLER                 PIC X(046).
      *                                                         309 046
         03  B074-LAYOUT-002-2  REDEFINES B074-LAYOUT-VARIAVEL-2.
      *                                                         308 047
             05 B074-CPR-VENDAS-002-2  PIC X(015).
             05 B074-OBR-FINANC-002-2  PIC X(007).
      *                                                         323 007
             05 B074-IDENT-ADM-002-2   PIC X(005).
      *                                                         330 005
             05 B074-VL-PRI-PRE-002-2  PIC 9(009)V99 COMP-3.
      *                                                         335 006
             05 B074-VL-DEM-PAR-002-2  PIC 9(009)V99 COMP-3.
      *                                                         341 006
             05 FILLER                 PIC X(008).
      *                                                         347 008
