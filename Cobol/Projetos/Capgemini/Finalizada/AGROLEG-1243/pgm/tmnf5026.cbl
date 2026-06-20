      *----------------------------------------------------------------*
       PROCESS TRUNC(BIN)
      *================================================================*
       IDENTIFICATION                  DIVISION.
      *================================================================*
       PROGRAM-ID.                     TMNF5026.
       AUTHOR.                         HERIBERTO GIANNASI.

      *================================================================*
      *                 P R I M E  -  I N F O R M A T I C A            *
      *----------------------------------------------------------------*
      *    PROGRAMA....: TMNF5026                                      *
      *    PROGRAMADOR.: HERIBERTO GIANNASI                            *
      *    ANALISTA....: HERIBERTO GIANNASI                            *
      *    DATA........: 29/03/2017                                    *
      *----------------------------------------------------------------*
      *    OBJETIVO.: REALIZA O ENVIO DAS MENSAGENS NAO-FINANCEIRAS    *
      *               RECEBIDAS DO ROTEADOR.                           *
      *----------------------------------------------------------------*
      *    BCO DE DADOS:                                               *
      *    DB2 TABLE                                   INCLUDE/BOOK    *
      *    TVLR_DISCT_CARAC                            TMNFB019        *
      *    TGRADE_HORA_INSTC                           TMNFB034        *
      *    TGRADE_INSTC_MSGEM                          TMNFB036        *
      *                                                                *
      *    TMNTRC_MSGRA_DMNGO                          TMNFB070        *
      *    TMNTRC_MSGRA_SEGDA                          TMNFB074        *
      *    TMNTRC_MSGRA_TRC                            TMNFB076        *
      *    TMNTRC_MSGRA_QUART                          TMNFB072        *
      *    TMNTRC_MSGRA_QTA                            TMNFB071        *
      *    TMNTRC_MSGRA_SEXTA                          TMNFB075        *
      *    TMNTRC_MSGRA_SBADO                          TMNFB073        *
      *    TMNTRC_MSGRA_CONTG                          TMNFB069        *
      *                                                                *
      *    TETAPA_MNTRC_DMNGO                          TMNFB062        *
      *    TETAPA_MNTRC_SEGDA                          TMNFB066        *
      *    TETAPA_MNTRC_TRC                            TMNFB068        *
      *    TETAPA_MNTRC_QUART                          TMNFB064        *
      *    TETAPA_MNTRC_QTA                            TMNFB063        *
      *    TETAPA_MNTRC_SEXTA                          TMNFB067        *
      *    TETAPA_MNTRC_SBADO                          TMNFB065        *
      *    TETAPA_MNTRC_CONTG                          TMNFB061        *
      *                                                                *
      *    TETAPA_ESTRT_DMNGO                          TMNFB054        *
      *    TETAPA_ESTRT_SEGDA                          TMNFB058        *
      *    TETAPA_ESTRT_TRC                            TMNFB060        *
      *    TETAPA_ESTRT_QUART                          TMNFB056        *
      *    TETAPA_ESTRT_QTA                            TMNFB055        *
      *    TETAPA_ESTRT_SEXTA                          TMNFB059        *
      *    TETAPA_ESTRT_SBADO                          TMNFB057        *
      *    TETAPA_ESTRT_CONTG                          TMNFB053        *
      *                                                                *
      *    TMSGEM_INCOT_DMNGO                          TMNFB078        *
      *    TMSGEM_INCOT_SEGDA                          TMNFB082        *
      *    TMSGEM_INCOT_TRC                            TMNFB084        *
      *    TMSGEM_INCOT_QUART                          TMNFB080        *
      *    TMSGEM_INCOT_QTA                            TMNFB079        *
      *    TMSGEM_INCOT_SEXTA                          TMNFB083        *
      *    TMSGEM_INCOT_SBADO                          TMNFB081        *
      *    TMSGEM_INCOT_CONTG                          TMNFB077        *
      *                                                                *
      *----------------------------------------------------------------*
      *    BOOKS ....:                                                 *
      *    TMNFW000 - LAYOUT DO MODULO                                 *
      *    TMNFWE16 - LAYOUT DO MODULO TMNF5020                        *
      *    TMNFWE09 - LAYOUT DO MODULO TMNF5020                        *
      *    TMNFWE05 - LAYOUT DO MODULO TMNF5020                        *
      *    TMNFWS01 - LAYOUT PADRAO SFN                                *
      *    TMNFWS02 - LAYOUT PADRAO SFN                                *
      *    TMNFWS04 - LAYOUT PADRAO SFN                                *
      *    TMNFWS05 - LAYOUT PADRAO SFN                                *
      *    TMNFW0C0 - LAYOUT GEN0006 SCOPUS                            *
      *    TMNFW0C3 - LAYOUT GEN0008 SCOPUS                            *
      *    I#POOLB6 - AREA DE COMUNICACAO COM O MODULO CDES0111        *
FLAVIO*    SCRYW301 - AREA DE COMUNICACAO COM O MODULO SCRY301O        *
      *    TMNFWERR - AREA DE TRATAMENTO DE ERROS                      *
      *----------------------------------------------------------------*
      *    MODULOS.:                                                   *
      *    CDES0111 - GERAR LOG DE ERROS                               *
      *    TMNF5240 - MODULO CONVERSOR DE TXT PARA XML                 *
FLAVIO*    SCRY301O - MODULO DE CRIPTOGRAFIA E DECRIPTOGRAFIA          *
      *================================================================*
030217*================================================================*
.     *                    A L T E R A C O E S                         *
.     *                    ===================                         *
.     *                                                                *
.     *    OBJETIVO: GRAVAR DADOS RELACIONADOS A MENSAGERIA PARA       *
.     *              EQUALIZAR OS DADOS TANTO DA BAIXA QUANTO DA ALTA. *
.     *    DATA:     03/02/2017                                        *
.     *    ANALISTA: MAURICIO MENDES - BSI TECNOLOGIA                  *
.     *                                                                *
030217*================================================================*
080318*================================================================*
.     *                    A L T E R A C O E S                         *
.     *                    ===================                         *
.     *                                                                *
.     *    OBJETIVO: RETIRAR TABELAS ANTIGAS DA MONITORACAO DO ENVIO   *
.     *              DE MENSAGENS                                      *
.     *    DATA:     08/03/2018                                        *
.     *    ANALISTA: HERIBERTO GIANNASI - PRIME                        *
.     *                                                                *
080318*================================================================*
CAT412*                    A L T E R A C O E S                         *
.     *                    ===================                         *
.     *                                                                *
.     *    OBJETIVO: MANUTENCAO DE CODIFICACAO PARA DOMINIO SPB02.     *
.     *    DATA:     08/11/2018                                        *
.     *    ANALISTA: 7COMM                                             *
.     *                                                                *
CAT412*================================================================*
181218*================================================================*
.     *                    A L T E R A C O E S                         *
.     *                    ===================                         *
.     *                                                                *
.     *    OBJETIVO: CHAMAR A CADEIA DO SCRY CONVERTIDO DE ASSEMBLER   *
.     *              P/ COBOL.                                         *
.     *    DATA:     18/12/2018                                        *
.     *    ANALISTA: LUCIANO NAGATA - 7COMM                            *
.     *                                                                *
181218*================================================================*
7C2604*================================================================*
7C2604*                         7  C  O  M  M                          *
7C2604*----------------------------------------------------------------*
7C2604*    PROGRAMADOR.: HERIBERTO GIANNASI                            *
7C2604*    ANALISTA....: HERIBERTO GIANNASI                            *
7C2604*    DATA........: 26/04/2025                                    *
7C2604*    DESCRICAO...: ANALISE DE IMPACTO MIGRACAO CNPJ              *
7C2604*                  - AJUSTE MOV DO SIMMC0 E SIMMC3               *
7C2604*================================================================*

      *================================================================*
       ENVIRONMENT                     DIVISION.
      *================================================================*
      *----------------------------------------------------------------*
       CONFIGURATION                   SECTION.
      *----------------------------------------------------------------*
       SPECIAL-NAMES.
           DECIMAL-POINT               IS COMMA.

      *================================================================*
       DATA                            DIVISION.
      *================================================================*
      *----------------------------------------------------------------*
       WORKING-STORAGE                 SECTION.
      *----------------------------------------------------------------*
      *----------------------------------------------------------------*
       77  FILLER                      PIC  X(026)         VALUE
           'INICIO DA WORKING TMNF5026'.
      *----------------------------------------------------------------*
      *----------------------------------------------------------------*
       77  FILLER                      PIC  X(016)         VALUE
           'AREAS AUXILIARES'.
      *----------------------------------------------------------------*
       01  WRK-ISPB-CIP-SILOC          PIC  X(08)          VALUE
           '29011780'.
       01  WRK-ISPB-CIP-ANTIGA         PIC  X(08)          VALUE
           '04391007'.

       01  WRK-NETAPA-ESTRT            PIC S9(18) COMP-3   VALUE ZEROS.

       01  WRK-TMSTP-CPTCAO.
           05 FILLER                   PIC  X(25)          VALUE SPACES.
           05 WRK-CPTCAO-AUX           PIC  9(01)          VALUE ZEROS.

       01  WRK-CPTCAO                  PIC S9(03) COMP-3   VALUE ZEROS.

       01  WRK-CINDCD-RPROC-AUX        PIC  9(01)          VALUE ZEROS.

       01  WRK-MF                      PIC S9(01) COMP-3   VALUE 2.
       01  WRK-ENVIO-LEGADO            PIC S9(01) COMP-3   VALUE 0.
       01  WRK-ENVIO-MANUAL            PIC S9(01) COMP-3   VALUE 2.

       01  WRK-EXECUTA-ROLLBACK        PIC  X(01)          VALUE SPACES.

       01  WRK-BASE-CONTG              PIC  X(01)          VALUE SPACES.
       01  WRK-MNTRC-HOST.
           05 WRK-HOST-CMNTRC-MSGRA    PIC  X(23)          VALUE SPACES.
           05 WRK-HOST-CSNLZD-MSGRA    PIC S9(03) COMP-3   VALUE ZEROS.
           05 WRK-HOST-CINDCD-ORIGE    PIC S9(01) COMP-3   VALUE ZEROS.
           05 WRK-HOST-CINDCD-PLATF    PIC S9(01) COMP-3   VALUE ZEROS.
           05 WRK-HOST-HPROCM-MSGEM    PIC  X(26)          VALUE SPACES.
           05 WRK-HOST-HULT-ATULZ      PIC  X(26)          VALUE SPACES.
           05 WRK-HOST-HMOVTO-INFO     PIC  X(26)          VALUE SPACES.
           05 WRK-HOST-CISPB-DSTNA     PIC  X(26)          VALUE SPACES.
           05 WRK-HOST-CISPB-EMISR     PIC  X(26)          VALUE SPACES.
           05 WRK-HOST-CCTRL-INSTC     PIC  X(20)          VALUE SPACES.
           05 WRK-HOST-CUNIC-TRFGO     PIC  X(23)          VALUE SPACES.
           05 WRK-HOST-RIDTFD-MSGEM    PIC  X(48)          VALUE SPACES.
           05 WRK-HOST-CINDCD-RPROC    PIC S9(01) COMP-3   VALUE ZEROS.
           05 WRK-HOST-CDOMNO-MSGRA    PIC  X(05)          VALUE SPACES.
           05 WRK-HOST-CSIST-EXTER     PIC  X(03)          VALUE SPACES.
           05 WRK-HOST-CEVNTO-MSGEM    PIC  X(04)          VALUE SPACES.
           05 WRK-HOST-CFLUXO-MSGEM    PIC  X(02)          VALUE SPACES.
           05 WRK-HOST-CFASE-PROCM     PIC S9(04) COMP-3   VALUE ZEROS.
           05 WRK-HOST-SIT-FASE        PIC S9(01) COMP-3   VALUE ZEROS.
           05 WRK-HOST-HINIC-ETAPA     PIC  X(26)          VALUE SPACES.
           05 WRK-HOST-HFNAL-ETAPA     PIC  X(26)          VALUE SPACES.
           05 WRK-HOST-RERRO-ETAPA     PIC  X(255)         VALUE SPACES.

       01  WRK-NULIDADE.
           05 WRK-NULL-01              PIC S9(04) COMP     VALUE ZEROS.
           05 WRK-NULL-02              PIC S9(04) COMP     VALUE ZEROS.
           05 WRK-NULL-03              PIC S9(04) COMP     VALUE ZEROS.
           05 WRK-NULL-04              PIC S9(04) COMP     VALUE ZEROS.
           05 WRK-NULL-05              PIC S9(04) COMP     VALUE ZEROS.
           05 WRK-NULL-06              PIC S9(04) COMP     VALUE ZEROS.
           05 WRK-NULL-07              PIC S9(04) COMP     VALUE ZEROS.
           05 WRK-NULL-08              PIC S9(04) COMP     VALUE ZEROS.

       01  WRK-DAYOFWEEK               PIC S9(01) COMP-3   VALUE ZEROS.
       01  WRK-AUX-S9-01               PIC +9(01)          VALUE ZEROS.
       01  FILLER                      REDEFINES WRK-AUX-S9-01.
           05 FILLER                   PIC  X(01).
           05 WRK-AUX-9-1              PIC  9(01).

       01  WRK-DIA-SEMANA              PIC  9(01)          VALUE ZEROS.
           88 WRK-DOMINGO                                  VALUE 1.
           88 WRK-SEGUNDA                                  VALUE 2.
           88 WRK-TERCA                                    VALUE 3.
           88 WRK-QUARTA                                   VALUE 4.
           88 WRK-QUINTA                                   VALUE 5.
           88 WRK-SEXTA                                    VALUE 6.
           88 WRK-SABADO                                   VALUE 7.

       01  WRK-X-1                     PIC  X              VALUE SPACES.
       01  FILLER                      REDEFINES WRK-X-1.
           05 WRK-9-1                  PIC  9.

PRIME *01  WRK-9-01                    PIC  9(001)         VALUE ZEROS.
.     *01  FILLER                      REDEFINES WRK-9-01.
PRIME *    05 WRK-X-01                 PIC  X(001).

       01  WRK-9-8                     PIC  9(008)         VALUE ZEROS.
       01  FILLER                      REDEFINES WRK-9-8.
           05 WRK-X-8                  PIC  X(008).

       01  WRK-PGM-CHAMADOR            PIC  X(08)          VALUE SPACES.
       01  WRK-COD-CERTIF              PIC  9(002)         VALUE ZEROS.
       01  FILLER                      REDEFINES WRK-COD-CERTIF.
           05 WRK-COD-CERTIF1          PIC  X(001).
           05 WRK-COD-CERTIF2          PIC  X(001).

PRIME *01  WRK-COD-CERT-INAT           PIC  X(002)         VALUE SPACES.
.     *01  FILLER                      REDEFINES WRK-COD-CERT-INAT.
.     *    05 WRK-COD-CERT-IN1         PIC  9(001).
PRIME *    05 WRK-COD-CERT-IN2         PIC  9(001).

       01  WRK-MSG.
           05 WRK-MSG-SIST             PIC  X(03)          VALUE SPACES.
           05 WRK-MSG-EVENTO           PIC  X(04)          VALUE SPACES.
           05 WRK-MSG-FLUXO            PIC  X(03)          VALUE SPACES.

       01  WRK-NATIONAL                PIC  N(32768)       VALUE SPACES.
       01  WRK-NATIONAL-X              REDEFINES WRK-NATIONAL
                                       PIC  X(65536).

CAT412 01  WRK-CCSID                   PIC  9(05)          VALUE ZEROS.
.          88 WRK-CCSID-ASCII                              VALUE 819.
.          88 WRK-CCSID-EBCDIC                             VALUE 37.
.          88 WRK-CCSID-UTF-8                              VALUE 1208.
CAT412     88 WRK-CCSID-UTF-16                             VALUE 1200.

       01  WRK-LEN-AREA-X.
           05 WRK-LEN-AREA-9           PIC  9(08)          VALUE ZEROS.

       01  WRK-TRATA-GRADE             PIC  X(01)          VALUE SPACES.
       01  WRK-PROGRAMA                PIC  X(08)          VALUE
           'TMNF5026'.

       01  WRK-TIMESTAMP-ZEROS         PIC  X(26)          VALUE
           '0001-01-01-00.00.00.000000'.

       01  WRK-CSNLZD-MSGRA            PIC  9(03)          VALUE ZEROS.
           88 WRK-CSNLZD-SUCESSO       VALUE 5.
           88 WRK-ERR-RECB-LEGADO      VALUE 3.
           88 WRK-ERR-PROCESSO         VALUE 4.

       01  WRK-DESC-WORKF.
           05 WRK-DESC-WORKF-LN1       PIC  X(79)          VALUE SPACES.
           05 WRK-DESC-WORKF-LN2       PIC  X(79)          VALUE SPACES.
           05 WRK-DESC-WORKF-LN3       PIC  X(79)          VALUE SPACES.
       01  WRK-MSG-PROBLEMA            PIC  X(01)          VALUE SPACES.

       01  WRK-S9-02                   PIC +9(02)          VALUE ZEROS.
       01  FILLER                      REDEFINES WRK-S9-02.
           05 FILLER                   PIC  X(01).
           05 WRK-9-02                 PIC  9(02).

       01  IND-1                       PIC  9(002) COMP-3  VALUE ZEROS.
       01  IND-2                       PIC S9(005) COMP-3  VALUE ZEROS.
       01  IND-3                       PIC S9(005) COMP-3  VALUE ZEROS.

       01  WRK-TAMANHO-REAL            PIC S9(08) COMP     VALUE ZEROS.
       01  FILLER                      REDEFINES WRK-TAMANHO-REAL.
           05 FILLER                   PIC  X(02).
           05 WRK-TAM-REAL-2           PIC S9(04)  COMP.

       01  WRK-MSG-A-ENVIAR            USAGE SQL TYPE IS CLOB(32768).
DAGO   01  WRK-MSG-TXT                 USAGE SQL TYPE IS CLOB(32768).
       01  WRK-MSG-XML                 USAGE SQL TYPE IS CLOB(32768).
DAGO  *01 XWRK-MSG-REPROC              USAGE SQL TYPE IS CLOB(66560).

REPROC 01  WRK-AREA-REPROC-TXT-ENV.
.          05 WRK-AREA-REPROC-DATAPOWER.
.             10 WRK-REPROC-CMNTRC     PIC  X(023)         VALUE SPACES.
.             10 WRK-REPROC-DOMINIO    PIC  X(005)         VALUE SPACES.
.             10 WRK-REPROC-FORMT-MSGEM
.                                      PIC  9(001)         VALUE ZEROS.
.             10 WRK-REPROC-FASE       PIC  9(001)         VALUE ZEROS.
.             10 WRK-REPROC-ISPB-E     PIC  X(008)         VALUE SPACES.
.             10 WRK-REPROC-ISPB-D     PIC  X(008)         VALUE SPACES.
.             10 FILLER                PIC  X(459)         VALUE SPACES.
.          05 WRK-REPROC-BCMSG         PIC  X(051)         VALUE SPACES.
.DAGO******05XWRK-REPROC-SISMSG        PIC  X(32768)       VALUE SPACES.
.
.      01  WRK-RPROC.
.          05 WRK-RPROC-CONVERSAO      PIC  X(001)         VALUE SPACES.
.          05 WRK-RPROC-CRIPTOGRAFIA   PIC  X(001)         VALUE SPACES.
.          05 WRK-RPROC-ENVIO-BACEN    PIC  X(001)         VALUE SPACES.
.
REPROC 01  WRK-ACHOU-CHAR              PIC  X(01)          VALUE SPACES.
FLAVIO 01  WRK-ERRO-COPIA              PIC  X(01)          VALUE SPACES.

       01  WRK-CEMISR-MSGEM            PIC  X(08)          VALUE SPACES.
       01  WRK-CEMISR-MSGEM-N          REDEFINES WRK-CEMISR-MSGEM
7C2604*                                P1C  9(08).
7C2604                                 PIC  X(08).
       01  WRK-CDSTNA-MSGEM            PIC  X(08)          VALUE SPACES.
       01  WRK-CDSTNA-MSGEM-N          REDEFINES WRK-CDSTNA-MSGEM
7C2604*                                P1C  9(08).
7C2604                                 PIC  X(08).
       01  WRK-CTRLIF                  PIC  X(20)          VALUE SPACES.

      *----------------------------------------------------------------*
       77  FILLER                      PIC  X(025)         VALUE
           'AREAS PARA ETAPAS'.
      *----------------------------------------------------------------*
       01 WRK-TMNFB029-NREG            PIC S9(02)  COMP-3  VALUE ZEROS.
AJG    01 WRK-TMNFB029-NREG-AUX        PIC S9(02)  COMP-3  VALUE ZEROS.
       01 WRK-LST-TMNFB029.
          05 WRK-CMNTRC-MSGRA          PIC  X(23)          OCCURS 12.
          05 WRK-NETAPA-MNTRC          PIC S9(02)  COMP-3  OCCURS 12.
          05 WRK-CFASE-PROCM           PIC S9(04)  COMP-3  OCCURS 12.
          05 WRK-SIT-FASE-B29          PIC S9(01)  COMP-3  OCCURS 12.
          05 WRK-HINIC-MNTRC           PIC  X(26)          OCCURS 12.
          05 WRK-HFNAL-MNTRC           PIC  X(26)          OCCURS 12.
          05 WRK-CUSUAR-MANUT          PIC  X(09)          OCCURS 12.
          05 WRK-CUSUAR-MANUT-N        PIC S9(04)  COMP    OCCURS 12.
          05 WRK-RERRO-MNTRC           PIC  X(255)         OCCURS 12.

      *----------------------------------------------------------------*
       01  FILLER                      PIC X(50)           VALUE
                   'AREA DE TRATAMENTO MQ            '.
      *----------------------------------------------------------------*
       01  WRK-MQ-MSGEM.
           05 FILLER                   PIC X(08)           VALUE
              'SBAT3520'.
           05 WRK-MQ-MSGEM-AREA        PIC X(82000)        VALUE SPACES.

       01  WRK-SBAT3520                PIC  X(008)         VALUE
           'SBAT3520'.

       77  WRK-BLANK                   PIC  X(01) VALUE SPACES.
       01  WRK-GETMAIN.
           03 WRK-LLBUFFER-GRAVACAO    PIC  9(09) COMP     VALUE ZEROS.
           03 WRK-PTR-GRAVACAO         USAGE IS POINTER VALUE NULL.
           03 FILLER                   REDEFINES WRK-PTR-GRAVACAO.
              05 WRK-PTR-GRAVACAO-COMP PIC  9(09) COMP.

       01 WRK-REP-DESPR-1              PIC X(10)           VALUE SPACES.
       01 WRK-REP-DESPR-2              PIC X(10)           VALUE SPACES.
       01 WRK-FILA-REPLY.
          05 FILLER                    PIC X(07)           VALUE
             'QL.REP.'.
          05 WRK-REP-ISPB-CMARA        PIC X(08)           VALUE SPACES.
          05 FILLER                    PIC X(01)           VALUE '.'.
          05 WRK-REP-ISPB-CONGL        PIC X(08)           VALUE SPACES.
          05 FILLER                    PIC X(01)           VALUE '.'.
          05 WRK-REP-SEQ               PIC X(02)           VALUE SPACES.
          05 FILLER                    PIC X(21)           VALUE SPACES.

       01 WRK-QM-REPLY                 PIC X(48)           VALUE SPACES.
       01 WRK-QM-SMP-REPLY.
          05 FILLER                    PIC X(03)           VALUE 'QM.'.
          05 WRK-QM-ISPB-CONGL         PIC X(08)           VALUE SPACES.
          05 FILLER                    PIC X(01)           VALUE '.'.
          05 WRK-QM-REPLY-SEQ          PIC X(02)           VALUE SPACES.
          05 FILLER                    PIC X(02)           VALUE SPACES.

       01 WRK-QM-CMP-REPLY.
          05 FILLER                    PIC X(03)           VALUE 'QM.'.
          05 WRK-QM-CMP-DEST           PIC X(08)           VALUE SPACES.
          05 FILLER                    PIC X(01)           VALUE '.'.
          05 WRK-QM-CMP-CONGL          PIC X(08)           VALUE SPACES.
          05 FILLER                    PIC X(01)           VALUE '.'.
          05 WRK-QM-CMP-SEQ            PIC X(02)           VALUE SPACES.

       01 WRK-NOTIFICACAO              PIC  9(008) COMP    VALUE ZEROS.
          88 WRK-SEM-NOTIF             VALUE ZEROS.
          88 WRK-COA                   VALUE 256.
          88 WRK-COD                   VALUE 2048.
          88 WRK-COA-COD               VALUE 2304.
          88 WRK-COA-COD-EXCPT         VALUE 16779520.

       01  WRK-MQ-FILA                 PIC  X(048)         VALUE SPACES.
       01  WRK-HANDLE-FILA             PIC S9(008) COMP    VALUE ZEROS.
       01  WRK-FUNCAO-OPCAO.
           05 WRK-OPEN                 PIC  X(010)         VALUE
              'OPEN'.
           05 WRK-CLOSE                PIC  X(010)         VALUE
              'CLOSE'.
           05 WRK-PUT                  PIC  X(010)         VALUE
              'PUT'.

       COPY 'SBATWC01'.

      *----------------------------------------------------------------*
       01  FILLER                      PIC X(50)           VALUE
                   'AREA DE TABELA INTERNA '.
      *----------------------------------------------------------------*
       01  WRK-FILAS-SBAT.
           05 WRK-FILA-CAMARA          PIC  X(048)         VALUE SPACES.
           05 WRK-HAND-CAMARA          PIC S9(008) COMP    VALUE ZEROS.
           05 WRK-FILA-OPEN            PIC  X(048)         VALUE SPACES.
           05 WRK-HAND-OPEN            PIC S9(008) COMP    VALUE ZEROS.
           05 WRK-FILA-RETORNO         PIC  X(048)         VALUE SPACES.
           05 WRK-HAND-RETORNO         PIC S9(008) COMP    VALUE ZEROS.

      *----------------------------------------------------------------*
       77  FILLER                      PIC  X(025)         VALUE
           'AREAS PARA NUM-UNICO-TMNF'.
      *----------------------------------------------------------------*
       01  WRK-TIMESTAMP               PIC  X(26)          VALUE SPACES.
       01  WRK-TSTAMP                  REDEFINES WRK-TIMESTAMP.
           05  WRK-TSTAMP-DATA.
               10  WRK-TSTAMP-AAAA     PIC  X(04).
               10  FILLER              PIC  X(01).
               10  WRK-TSTAMP-MM       PIC  X(02).
               10  FILLER              PIC  X(01).
               10  WRK-TSTAMP-DD       PIC  X(02).
           05  WRK-TSTAMP-HIFEN        PIC  X(01).
           05  WRK-TSTAMP-HH           PIC  X(02).
           05  FILLER                  PIC  X(01).
           05  WRK-TSTAMP-MIN          PIC  X(02).
           05  FILLER                  PIC  X(01).
           05  WRK-TSTAMP-SS           PIC  X(02).
           05  FILLER                  PIC  X(07).
030217 01  FILLER                      REDEFINES WRK-TIMESTAMP.
030217     05  FILLER                  PIC  X(24).
030217     05  WRK-TSTAMP-ML           PIC  9(02).

       01  WRK-NUM-UNICO-TMNF.
           05  WRK-TSTAMP-DATA.
               10  WRK-TSTAMP-AAAA     PIC  X(04)          VALUE SPACES.
               10  WRK-TSTAMP-MM       PIC  X(02)          VALUE SPACES.
               10  WRK-TSTAMP-DD       PIC  X(02)          VALUE SPACES.
           05  WRK-TSTAMP-HIFEN        PIC  X(01)          VALUE SPACES.
           05  WRK-TSTAMP-HH           PIC  X(02)          VALUE SPACES.
           05  WRK-TSTAMP-MIN          PIC  X(02)          VALUE SPACES.
           05  WRK-TSTAMP-SS           PIC  X(02)          VALUE SPACES.
           05  WRK-TSTAMP-SEQ          PIC  9(08)          VALUE ZEROS.

       01  WRK-SEQUENCIA               PIC S9(08)  COMP-3  VALUE ZEROS.

       01  WRK-SEQ-TIRA-SINAL          PIC 99999999-       VALUE ZEROS.
       01  FILLER                      REDEFINES WRK-SEQ-TIRA-SINAL.
           05  WRK-SEQ-SEM-SINAL       PIC 99999999.
           05  WRK-SINAL               PIC X.

.      01  WRK-ERRO-NP50.
.          05  FILLER                  PIC  X(043)         VALUE
.          'ERRO NO START DA TRANSACAO NP50 - EIBRESP: '.
.          05  WRK-ERRO-NP50-EIBRESP   PIC  9(008)         VALUE ZEROS.
.
.      01  WRK-ERRO-NP60.
.          05  FILLER                  PIC  X(043)         VALUE
.          'ERRO NO START DA TRANSACAO NP60 - EIBRESP: '.
.          05  WRK-ERRO-NP60-EIBRESP   PIC  9(008)         VALUE ZEROS.
.
.     *----------------------------------------------------------------*
.      01  FILLER                      PIC  X(050)         VALUE
.          'AREA DE DADOS PARA BASE TEMPORARIA'.
.     *----------------------------------------------------------------*
.      01  WRK-TAM-DADOS-BASE-TEMP     PIC  9(06) COMP-3   VALUE ZEROS.
.      01  WRK-NOME-TS.
.          05  WRK-NOME-CCUSTO         PIC  X(004)         VALUE 'SPBN'.
.          05  WRK-NOME-TRX            PIC  X(004)         VALUE 'NP50'.
.          05  WRK-TERM-TS             PIC  X(004)         VALUE SPACES.
.
.      01  WRK-DADOS-TS                PIC  X(32000)       VALUE SPACES.
.      01  WRK-TAM-DADOS-TS            PIC  9(006) COMP-3  VALUE ZEROS.
.
.      01  WRK-ERRO-DEL-TS.
.          05  FILLER                  PIC  X(028)         VALUE
.          'TRX NP50 - ERRO DELECAO TS: '.
.          05  WRK-ERRO-NP50-RESP-1    PIC  9(008)         VALUE ZEROS.
.
.      01  WRK-ERRO-GRA-TS.
.          05  FILLER                  PIC  X(029)         VALUE
.          'TRX NP50 - ERRO GRAVACAO TS: '.
.          05  WRK-ERRO-NP50-RESP-2    PIC  9(008)         VALUE ZEROS.
.
.      01  WRK-ERRO-SPBN               PIC  X(001)         VALUE SPACES.
.          88  WRK-ERRO-SPBN-S         VALUE 'S'.
.          88  WRK-ERRO-SPBN-N         VALUE 'N'.

070518 COPY SPBNWNP5.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           'AREA DE DADOS PARA BASE TEMPORARIA'.
      *----------------------------------------------------------------*
       01  WRK-AREA-SPBN5001           PIC  X(32000)       VALUE SPACES.

      *----------------------------------------------------------------*
       77  FILLER                      PIC  X(022)         VALUE
           'AREAS TEXTO ERROS'.
      *----------------------------------------------------------------*
       01  WRK-MSG-ENTRADA-MANUAL.
           05 FILLER                   PIC  X(05)          VALUE 'ERRO'.
           05 WRK-ERR-EM-PGM           PIC  X(08)          VALUE SPACES.
           05 FILLER                   PIC  X(04)          VALUE 'LOC:'.
           05 WRK-ERR-EM-LOCAL         PIC  X(04)          VALUE SPACES.
           05 WRK-ERRO-GERAL           PIC  X(56)          VALUE SPACES.
           05 WRK-ERR-DB2-MNUAL        REDEFINES WRK-ERRO-GERAL.
              10 WRK-ERR-TABELA        PIC  X(18).
              10 WRK-ERR-COMANDO       PIC  X(10).
              10 WRK-ERR-SQLCOD        PIC  -999.
              10 FILLER                PIC  X(24).
           05  WRK-ERR-CICS-MNUAL      REDEFINES WRK-ERRO-GERAL.
               10  WRK-ERR-K-EIBR      PIC  X(08).
               10  WRK-ERR-EIBR        PIC  X(02).
               10  WRK-ERR-K-MODUL     PIC  X(09).
               10  WRK-ERR-MODUL       PIC  X(08).
               10  FILLER              PIC  X(29).

       01  WRK-ERR-ABEND.
           05 FILLER                   PIC  X(12)          VALUE
              'ABEND CODE: '.
           05 WRK-ABCODE               PIC  X(04)          VALUE SPACES.
           05 FILLER                   PIC  X(09)          VALUE
              ' ROTINA: '.
           05 WRK-PARAGRAFO            PIC  X(30)          VALUE SPACES.

       01  WRK-ERR-MQ.
           05 FILLER                   PIC  X(13)          VALUE
                                       'ERRO MQ-FILA:'.
           05 WRK-ERR-MQ-FILA          PIC  X(40)          VALUE SPACES.
           05 FILLER                   PIC  X(01)          VALUE '/'.
           05 WRK-ERR-MQ-REASON        PIC  X(08)          VALUE SPACES.
           05 FILLER                   PIC  X(01)          VALUE '/'.
           05 WRK-ERR-MQ-CMD           PIC  X(03)          VALUE SPACES.
           05 FILLER                   PIC  X(01)          VALUE '/'.
           05 WRK-ERR-MQ-LOCAL         PIC  X(04)          VALUE SPACES.

       01  WRK-MSG-ERR-APL             PIC  X(79)          VALUE SPACES.
       01  WRK-ERR-DB2.
           05 FILLER                   PIC  X(18)          VALUE
              'ERRO DB2 - LOCAL: '.
           05 WRK-ERR-DB2-LOCAL        PIC  X(04)          VALUE ZEROS.
           05 FILLER                   PIC  X(10)          VALUE
              ' SQLCODE: '.
           05 WRK-ERR-DB2-SQLCODE      PIC -999B           VALUE ZEROS.
           05 FILLER                   PIC  X(09)          VALUE
              ' SQLCA: '.
           05 WRK-ERR-DB2-SQLCA        PIC  X(30)          VALUE SPACES.

       01  WRK-ERR-CICS.
           05 FILLER                   PIC  X(19)          VALUE
              'ERRO CICS - LOCAL: '.
           05 WRK-ERR-CICS-LOCAL       PIC  X(04)          VALUE ZEROS.
           05 FILLER                   PIC  X(10)          VALUE
              ' EIBRESP: '.
           05 WRK-ERR-CICS-EIBRESP     PIC +9(08)          VALUE ZEROS.
           05 FILLER                   PIC  X(11)          VALUE
              ' EIBRESP2: '.
           05 WRK-ERR-CICS-EIBRESP2    PIC +9(08)          VALUE ZEROS.

      *----------------------------------------------------------------*
       77  FILLER                      PIC  X(018)     VALUE
           'AREA PARA CDES0111'.
      *----------------------------------------------------------------*
       COPY 'I#POOLB6'.

      *----------------------------------------------------------------*
       77  FILLER                      PIC  X(009)     VALUE
           'NULIDADES'.
      *----------------------------------------------------------------*

       01  WRK-NULIDADES-B027.
           05  WRK-NULL-HRECBD-MSGEM       PIC S9(04)  COMP VALUE -1.
           05  WRK-NULL-HCHEGD-MSGEM       PIC S9(04)  COMP VALUE -1.

       01  WRK-NULIDADES-B029.
           05  WRK-NULL-RERRO-ETAPA-MNTRC  PIC S9(04)  COMP VALUE -1.

       01  WRK-NULIDADES-B039.
           05 WRK-CMSGRA-SIST-NULL         PIC S9(04)  COMP VALUE ZEROS.
           05 WRK-CMSGEM-INCOT-NULL        PIC S9(04)  COMP VALUE ZEROS.
           05 WRK-CIDTFD-INCON-NULL        PIC S9(04)  COMP VALUE ZEROS.
           05 WRK-CIDTFD-FLUXO-NULL        PIC S9(04)  COMP VALUE ZEROS.
           05 WRK-IFILA-MSGRA-NULL         PIC S9(04)  COMP VALUE ZEROS.
           05 WRK-RERRO-ETAPA-NULL         PIC S9(04)  COMP VALUE ZEROS.
           05 WRK-RCONTD-MSGEM-NULL        PIC S9(04)  COMP VALUE ZEROS.
           05 WRK-WCONTD-MSGEM-NULL        PIC S9(04)  COMP VALUE ZEROS.

      *----------------------------------------------------------------*
       01  FILLER                      PIC X(32)           VALUE
                   'AREA DE LAYOUT ENVIO OPEN'.
      *----------------------------------------------------------------*
       COPY TMNFWO01.

      *----------------------------------------------------------------*
       01  FILLER                      PIC X(32)           VALUE
                   'AREA DE LAYOUT PADRAO SFN'.
      *----------------------------------------------------------------*
       01  WRK-BLOCO-FINANCEIRO.
           COPY TMNFWS02.

       01  WRK-BLOCO-RETORNO.
           COPY TMNFWS03.

      *----------------------------------------------------------------*
       01  FILLER                      PIC X(32)           VALUE
                   'AREA DE LAYOUT GEN0006 SCOPUS'.
      *----------------------------------------------------------------*
       COPY 'TMNFW0C0'.

      *----------------------------------------------------------------*
       01  FILLER                      PIC X(32)           VALUE
                   'AREA DE LAYOUT GEN0008 SCOPUS'.
      *----------------------------------------------------------------*
       COPY 'TMNFW0C3'.

      *----------------------------------------------------------------*
       01  FILLER                      PIC X(32)           VALUE
                   'AREA DE LAYOUT GEN0018 SCOPUS'.
      *----------------------------------------------------------------*
       COPY 'TMNFW0D0'.

      *----------------------------------------------------------------*
       01  FILLER                      PIC X(32)           VALUE
                   'AREA DE COMUNICACAO DATAPOWER'.
      *----------------------------------------------------------------*
RET    01  WRK-AREA-BLOCO-RETORNO.
RET        COPY TMNFWE11.

FLAVIO*----------------------------------------------------------------*
FLAVIO 01  FILLER                      PIC X(50)           VALUE
FLAVIO     'AREA DE COMUNICACAO SCRY301O'.
FLAVIO*----------------------------------------------------------------*
       01  WRK-SCRY301O                PIC X(08)           VALUE
           'SCRY301O'.

       01  WRK-AREA-SCRY301O-END       USAGE IS POINTER    VALUE NULL.
       01  WRK-AREA-SCRY301O.
           COPY 'TMNFW998'.

      *----------------------------------------------------------------*
       01  FILLER                      PIC X(50)           VALUE
           'AREA DE COMUNICACAO NP60    '.
      *----------------------------------------------------------------*
       01  WRK-NP60-AREA.
           COPY SPBNW504.

      *----------------------------------------------------------------*
       01  FILLER                      PIC X(50)           VALUE
           'AREA DE COMUNICACAO TMNF5040 - CONVERSOR XML'.
      *----------------------------------------------------------------*
       01  WRK-TMNF5040                PIC X(008)          VALUE
           'TMNF5040'.

       01  WRK-AREA-TMNF5040.
           05 FILLER                   PIC X(32763)        VALUE SPACES.

       01  WRK-TMNF5026-ENTRADA.
           COPY TMNFWS04.
           05 WRK-ENT-DOM-SIST         PIC  X(05)          VALUE SPACES.
           COPY TMNFWS05.

       01  WRK-AREA-SAIDA-TMNF5040.
           COPY TMNFW820.

      *----------------------------------------------------------------*
       01  FILLER                      PIC X(50)           VALUE
           'AREA DE COMUNICACAO TMNF5290 - CONV MQID'.
      *----------------------------------------------------------------*
       01  WRK-TMNF5290                PIC X(008)          VALUE
           'TMNF5290'.

       01  WRK-AREA-TMNF5290.
           COPY TMNFWE04.

      *----------------------------------------------------------------*
       01  FILLER                      PIC X(050)          VALUE
           'INTERF. NUOP, NUMCTRLIF E GRADE'.
      *----------------------------------------------------------------*
       01  WRK-NUOP                    PIC  X(23)          VALUE SPACES.
       01  WRK-NUOP-OUTROS.
           05  WRK-NUOP-ISPB           PIC  X(08)          VALUE SPACES.
           05  WRK-NUOP-DATA           PIC  X(08)          VALUE SPACES.
           05  WRK-NUOP-SEQ            PIC  9(07)          VALUE ZEROS.

       01  WRK-NUOP-SPB02.
           05  WRK-NUOP-ISPB-SPB02     PIC  X(08)          VALUE SPACES.
           05  WRK-NUOP-DATA-SPB02     PIC  X(06)          VALUE SPACES.
           05  WRK-NUOP-SEQ-SPB02      PIC  9(09)          VALUE ZEROS.

       01  WRK-SEQUEN-NUOP-SPB02       PIC S9(09) COMP-3   VALUE ZEROS.
       01  WRK-SEQUEN-NUOP             PIC S9(07) COMP-3   VALUE ZEROS.

       01  WRK-NUOP-TIRA-SINAL         PIC  9999999-       VALUE ZEROS.
       01  FILLER REDEFINES WRK-NUOP-TIRA-SINAL.
           05  WRK-NUOP-SEM-SINAL      PIC  9999999.
           05  FILLER                  PIC  X.
       01  WRK-NUOP-TIRA-SINAL-SPB02   PIC  999999999-     VALUE ZEROS.
       01  FILLER REDEFINES WRK-NUOP-TIRA-SINAL-SPB02.
           05  WRK-NUOP-SEM-SINAL-SPB02    PIC  999999999.
           05  FILLER                      PIC  X.

       01  FILLER.
           05  WRK-SQL-CODE            PIC S9(08)          VALUE ZEROS.
           05  FILLER                  REDEFINES WRK-SQL-CODE.
               10  FILLER              PIC  X(5).
               10  WRK-SQL-CODE-3      PIC S9(3).

       01  FILLER.
           05  WRK-FASE                PIC  9(4)           VALUE ZEROS.
           05  WRK-SIT-FASE            PIC  9(1)           VALUE ZEROS.
      *----------------------------------------------------------------*
       77  FILLER                      PIC  X(32)          VALUE
           '*         AREA DE DB2          *'.
      *----------------------------------------------------------------*

           EXEC SQL
               INCLUDE SQLCA
           END-EXEC.

           EXEC SQL
               INCLUDE TMNFB009
           END-EXEC.

080318     EXEC SQL
   .           INCLUDE TMNFB078
080318     END-EXEC.

           EXEC SQL
               INCLUDE TMNFB032
           END-EXEC.

           EXEC SQL
               INCLUDE TMNFB031
           END-EXEC.

           EXEC SQL
               INCLUDE TMNFB019
           END-EXEC.

           EXEC SQL
               INCLUDE TMNFB034
           END-EXEC.

           EXEC SQL
               INCLUDE TMNFB036
           END-EXEC.

           EXEC SQL
               INCLUDE TMNFB053
           END-EXEC

181218*----------------------------------------------------------------*
.     **** AREAS PARA TRATAR MODULO SCRY CONV DE ASSEMBLER P/ COBOL
.     *----------------------------------------------------------------*
.      01  WRK-SCRY-CONV               PIC  X(80)          VALUE SPACES.
.      01  FILLER        REDEFINES WRK-SCRY-CONV.
.          03 WRK-SCRY-CONV-DOM        PIC  X(01).
.          03 FILLER                   PIC  X(79).
.
.      01  WRK-CVLR-DISCT              PIC  X(15)          VALUE SPACES.
.      01  FILLER        REDEFINES WRK-CVLR-DISCT.
.          03 WRK-CVLR-DISCT-1         PIC  X(06).
.          03 WRK-CVLR-DISCT-2         PIC  X(09).
.
.      01  WRK-SCRY331O                PIC X(08)           VALUE
.          'SCRY331O'.
.
.      01  WRK-AREA-SCRY331O-END       USAGE IS POINTER    VALUE NULL.
.      01  WRK-AREA-SCRY331O.
181218     COPY 'TMNFW997'.

      *----------------------------------------------------------------*
       77  FILLER                      PIC  X(023)         VALUE
           'FIM DA WORKING TMNF5026'.
      *----------------------------------------------------------------*
      *----------------------------------------------------------------*
       LINKAGE                         SECTION.
      *----------------------------------------------------------------*
       01  DFHCOMMAREA.
       COPY 'TMNFWE16'.
       COPY 'TMNFWE09'.
       COPY 'TMNFWE05'.

      *================================================================*
       PROCEDURE                       DIVISION USING DFHCOMMAREA.
      *================================================================*
      *----------------------------------------------------------------*
       0000-ROTINA-PRINCIPAL           SECTION.
      *----------------------------------------------------------------*
           EXEC  CICS HANDLE ABEND LABEL (9800-TRATAR-ABEND)
           END-EXEC.

           EXEC  CICS ASSIGN INVOKINGPROG (WRK-PGM-CHAMADOR)
           END-EXEC.

           PERFORM 1000-INICIALIZAR.

           PERFORM 2000-PROCESSAR.

           PERFORM 3000-FINALIZAR.

      *----------------------------------------------------------------*
       0000-99-FIM.                    EXIT.
      *----------------------------------------------------------------*
      *----------------------------------------------------------------*
       1000-INICIALIZAR                SECTION.
      *----------------------------------------------------------------*
           INITIALIZE ERRO-AREA
                      WRK-CEMISR-MSGEM
                      WRK-CDSTNA-MSGEM
                      WRK-CTRLIF
                      WRK-NUOP
                      WRK-NUM-UNICO-TMNF
                      WRK-MSG-ERR-APL
                      WRK-CINDCD-RPROC-AUX.

           SET WRK-CSNLZD-SUCESSO      TO TRUE.

REPROC     MOVE 'N'                    TO WRK-RPROC-CONVERSAO
.                                         WRK-RPROC-CRIPTOGRAFIA
REPROC                                    WRK-RPROC-ENVIO-BACEN.

           MOVE 'N'                    TO WRK-MSG-PROBLEMA.
FLAVIO     MOVE 'N'                    TO WRK-ERRO-COPIA.

           PERFORM 1100-MOVER-ETAPA-INCIAL.

           PERFORM 1300-OBTER-DIA-SEMANA.

           PERFORM 1400-GETMAIN-AREA-GRV.

           MOVE WRK-TIMESTAMP          TO WRK-TMSTP-CPTCAO.
           MOVE WRK-CPTCAO-AUX         TO WRK-CPTCAO.

      *---------------------------------------------------------------*
       1000-99-FIM.                    EXIT.
      *----------------------------------------------------------------*
      *----------------------------------------------------------------*
       1100-MOVER-ETAPA-INCIAL         SECTION.
      *----------------------------------------------------------------*

           PERFORM 1110-MOVER-ETAPA-RECEP.

           PERFORM 1120-MOVER-ETAPA-INICIO.

      *----------------------------------------------------------------*
       1100-99-EXIT.                   EXIT.
      *----------------------------------------------------------------*
      *----------------------------------------------------------------*
       1110-MOVER-ETAPA-RECEP          SECTION.
      *----------------------------------------------------------------*

           MOVE 1                      TO WRK-TMNFB029-NREG
AJG                                       WRK-TMNFB029-NREG-AUX.

           IF  TMNFWE16-ENTRADA-MANUAL EQUAL 'S'
               MOVE 79                 TO WRK-CFASE-PROCM
                                         (WRK-TMNFB029-NREG)
           ELSE
               MOVE 55                 TO WRK-CFASE-PROCM
                                         (WRK-TMNFB029-NREG)
           END-IF

           MOVE 2                      TO WRK-SIT-FASE-B29
                                         (WRK-TMNFB029-NREG)

           MOVE TMNFWE16-HOCORR-WKFLW  TO WRK-HINIC-MNTRC
                                         (WRK-TMNFB029-NREG)

           PERFORM 1200-OBTER-DATA-HORA.
           MOVE WRK-TIMESTAMP          TO WRK-HFNAL-MNTRC
                                         (WRK-TMNFB029-NREG)

           MOVE ZEROS                  TO WRK-NULL-RERRO-ETAPA-MNTRC
           MOVE SPACES                 TO WRK-RERRO-MNTRC
                                         (WRK-TMNFB029-NREG)

           IF TMNFWE16-ENTRADA-MANUAL  EQUAL 'S'
               MOVE 'RECEPCAO PELO '   TO WRK-RERRO-MNTRC
                                         (WRK-TMNFB029-NREG)
               MOVE TMNFWE16-FILA-LEGADO
                                       TO WRK-RERRO-MNTRC
                                         (WRK-TMNFB029-NREG)(16:48)
           ELSE
               MOVE 'RECEPCAO PELA FILA '
                                       TO WRK-RERRO-MNTRC
                                         (WRK-TMNFB029-NREG)
               MOVE TMNFWE16-FILA-LEGADO
                                       TO WRK-RERRO-MNTRC
                                         (WRK-TMNFB029-NREG)(20:48)
           END-IF.

      *----------------------------------------------------------------*
       1110-99-EXIT.                   EXIT.
      *----------------------------------------------------------------*
      *----------------------------------------------------------------*
       1120-MOVER-ETAPA-INICIO         SECTION.
      *----------------------------------------------------------------*
AJG        MOVE WRK-TMNFB029-NREG      TO WRK-TMNFB029-NREG-AUX
AJG        ADD 1                       TO WRK-TMNFB029-NREG-AUX
AJG        MOVE WRK-TMNFB029-NREG-AUX  TO WRK-TMNFB029-NREG.

           MOVE 27                     TO WRK-CFASE-PROCM
                                         (WRK-TMNFB029-NREG)

           MOVE 2                      TO WRK-SIT-FASE-B29
                                         (WRK-TMNFB029-NREG)

           MOVE WRK-TIMESTAMP          TO WRK-HINIC-MNTRC
                                         (WRK-TMNFB029-NREG)

           PERFORM 1200-OBTER-DATA-HORA.
           MOVE WRK-TIMESTAMP          TO WRK-HFNAL-MNTRC
                                         (WRK-TMNFB029-NREG)

           MOVE ZEROS                  TO WRK-NULL-RERRO-ETAPA-MNTRC
           MOVE SPACES                 TO WRK-RERRO-MNTRC
                                         (WRK-TMNFB029-NREG)
           MOVE 'INICIO PROCESSAMENTO MENSAGERIA V2'
                                       TO WRK-RERRO-MNTRC
                                         (WRK-TMNFB029-NREG).

      *----------------------------------------------------------------*
       1120-99-EXIT.                   EXIT.
      *----------------------------------------------------------------*
      *----------------------------------------------------------------*
       1200-OBTER-DATA-HORA            SECTION.
      *----------------------------------------------------------------*
           EXEC SQL
                SET :WRK-TIMESTAMP     = CURRENT_TIMESTAMP
           END-EXEC

           IF  SQLCODE                 NOT EQUAL ZEROS
               INITIALIZE ERRO-AREA
               MOVE 'TIMESTAMP'        TO ERR-DBD-TAB
               MOVE 'SET'              TO ERR-FUN-COMANDO
               MOVE '0005'             TO ERR-LOCAL
               PERFORM 9200-TRATAR-ERRO-DB2
               PERFORM 3300-PROCEDIMENTOS-FINAIS
           END-IF.

      *----------------------------------------------------------------*
       1200-99-EXIT.                   EXIT.
      *----------------------------------------------------------------*
      *----------------------------------------------------------------*
       1300-OBTER-DIA-SEMANA           SECTION.
      *----------------------------------------------------------------*
           EXEC SQL
              SET :WRK-DAYOFWEEK = DAYOFWEEK(CURRENT_DATE)
           END-EXEC.

           IF SQLCODE                  NOT EQUAL ZEROS
              MOVE 8                   TO WRK-DAYOFWEEK
           END-IF.

           MOVE WRK-DAYOFWEEK          TO WRK-AUX-S9-01
           MOVE WRK-AUX-9-1            TO WRK-DIA-SEMANA.

      *----------------------------------------------------------------*
       1300-99-FIM.                    EXIT.
      *----------------------------------------------------------------*
      *----------------------------------------------------------------*
       1400-GETMAIN-AREA-GRV           SECTION.
      *----------------------------------------------------------------*

           EXEC CICS GETMAIN SET (WRK-PTR-GRAVACAO)
                             FLENGTH (LENGTH OF WRK-MQ-MSGEM)
                             INITIMG (WRK-BLANK)
                             NOHANDLE
           END-EXEC.

           IF EIBRESP                 NOT EQUAL DFHRESP(NORMAL)
              MOVE 'TMNF5026'         TO ERR-MODULO
              MOVE '1400'             TO ERR-LOCAL
              PERFORM 9300-TRATAR-ERRO-CICS
              PERFORM 3000-FINALIZAR
           END-IF.

      *----------------------------------------------------------------*
       1400-99-EXIT.                   EXIT.
      *----------------------------------------------------------------*
      *----------------------------------------------------------------*
       2000-PROCESSAR                  SECTION.
      *----------------------------------------------------------------*
           PERFORM 2100-TRATAR-MONITORACAO.

           PERFORM 2200-CONVERSAO-TXT-XML.

260318*   XPERFORM 2300-CONSISTE-NUMCTRLIF.

           PERFORM 2600-TRATAR-GRADE-HORARIA.

181218     MOVE '0'                    TO WRK-SCRY-CONV-DOM
.
.          PERFORM 2720-TRATAR-VALOR-DISCR
.          MOVE RVLR-DISCT-CARAC       OF TMNFB019
.                                      TO WRK-SCRY-CONV
.
.          IF WRK-SCRY-CONV-DOM        EQUAL '1'
.             PERFORM 2730-TRATAR-CRIPTO-N
.          ELSE
.             PERFORM 2700-TRATAR-CRIPTO
181218     END-IF.

           PERFORM 2800-TRATAR-ENVIO-CMARA.

           PERFORM 2900-ENVIAR-COPIA-OPEN.

           MOVE ZEROS                  TO ERR-RETURN-CODE.

      *----------------------------------------------------------------*
       2000-99-FIM.                    EXIT.
      *----------------------------------------------------------------*
      *----------------------------------------------------------------*
       2100-TRATAR-MONITORACAO         SECTION.
      *----------------------------------------------------------------*
           PERFORM 2110-MOVER-DADOS-INICIAIS.

           PERFORM 2120-GERAR-NUM-TMNF.

           PERFORM 2140-GERAR-NUM-OP

           EVALUATE TMNFWE16-CODMSG
               WHEN 'GEN0006  '
                  MOVE WRK-NUOP        TO SIMMC0-NUOP
                                          TMNFWS04-CNRO-UNIC-OPER
                  MOVE SIMMC0-REGISTRO TO TMNFWE05-SIMMC0
               WHEN 'GEN0008  '
                  MOVE WRK-NUOP        TO SIMMC3-NUOP
                                          TMNFWS04-CNRO-UNIC-OPER
                  MOVE SIMMC3-REGISTRO TO TMNFWE05-SIMMC3
               WHEN 'GEN0018  '
                  MOVE WRK-NUOP        TO SIMMD0-NUOP
                  MOVE SIMMD0-REGISTRO TO TMNFWE05-SIMMD0
               WHEN OTHER
                  MOVE WRK-NUOP        TO TMNFWS04-CNRO-UNIC-OPER
                  MOVE TMNFWS04-BLOCO-BCMSG
                                       TO TMNFWE05-SIMM04
           END-EVALUATE.

      *----------------------------------------------------------------*
       2100-99-FIM.                    EXIT.
      *----------------------------------------------------------------*
      *----------------------------------------------------------------*
       2110-MOVER-DADOS-INICIAIS       SECTION.
      *----------------------------------------------------------------*
           INITIALIZE WRK-TMNF5026-ENTRADA.

           MOVE  TMNFWE16-CODMSG       TO WRK-MSG

           EVALUATE TMNFWE16-CODMSG
               WHEN 'GEN0006  '
                  MOVE TMNFWE05-SIMMC0 TO SIMMC0-REGISTRO
                  MOVE SIMMC0-ID-EMISSOR
                                       TO WRK-CEMISR-MSGEM-N
                  MOVE SIMMC0-ID-DESTI TO WRK-CDSTNA-MSGEM-N
                  MOVE SIMMC0-NUMCTRLIF
                                       TO WRK-CTRLIF
                  MOVE SIMMC0-ID-EMISSOR
                                       TO WRK-9-8
                  MOVE WRK-X-8         TO TMNFWS04-CSIST-PGTO-EMISR
                  MOVE SIMMC0-ID-DESTI TO WRK-9-8
                  MOVE WRK-X-8         TO TMNFWS04-CSIST-PGTO-DSTNA
                  MOVE SIMMC0-NUMSEQ   TO TMNFWS04-CNRO-SEQ-BACEN
                  MOVE SIMMC0-INDCONT  TO TMNFWS04-CINDCD-CONTINUA
                  MOVE SIMMC0-NUOP     TO TMNFWS04-CNRO-UNIC-OPER

                  IF SIMMC0-CODCERTIFRATV
                                       NOT NUMERIC
                     MOVE SIMMC0-CODCERTIFRATV
                                       TO WRK-COD-CERTIF
                     MOVE WRK-COD-CERTIF1
                                       TO WRK-X-1
                     MOVE ZEROS        TO SIMMC0-CODCERTIFRATV
                     MOVE WRK-9-1      TO SIMMC0-CODCERTIFRATV
                  END-IF

PRIME *           SE SIMMC0-CODCERTIFRINATV
.     *                                NAO NUMERICO
.     *              MOVER SIMMC0-CODCERTIFRINATV
.     *                                PARA WRK-COD-CERT-INAT
.     *              SE WRK-COD-CERT-IN1
.     *                                EH NUMERICO
.     *                 MOVER TODOS ZEROS PARA SIMMC0-CODCERTIFRINATV
.     *                 MOVER WRK-COD-CERT-IN1
.     *                                PARA WRK-9-01
.     *                 MOVER WRK-X-01 PARA SIMMC0-CODCERTIFRINATV(2:1)
.     *              FIM-SE
PRIME *           FIM-SE

                  STRING  SIMMC0-CODMSG
                          SIMMC0-NUMCTRLIF
                          SIMMC0-ISPBIF
                          SIMMC0-CODCERTIFRATV
                          SIMMC0-CERTIFATV
PRIME *                   SIMMC0-SITCERTIF
PRIME *                   SIMMC0-CODCERTIFRINATV
PRIME *                   SIMMC0-CERTIFINATV
                          SIMMC0-HIST
                          SIMMC0-DTMOVTO
                          DELIMITED BY SIZE INTO TMNFWS05-MSG

                  MOVE 'SPB02'         TO WRK-ENT-DOM-SIST

               WHEN 'GEN0008  '
                  MOVE TMNFWE05-SIMMC3 TO SIMMC3-REGISTRO
                  MOVE SIMMC3-ID-EMISSOR
                                       TO WRK-CEMISR-MSGEM-N
                  MOVE SIMMC3-ID-DESTI TO WRK-CDSTNA-MSGEM-N
                  MOVE SIMMC3-NUMCTRLIF
                                       TO WRK-CTRLIF

                  MOVE SIMMC3-TIPOID-EMISSOR
                                       TO TMNFWS04-CEMISR-MSGEM
                  MOVE SIMMC3-ID-EMISSOR
                                       TO WRK-9-8
                  MOVE WRK-X-8         TO TMNFWS04-CSIST-PGTO-EMISR
                  MOVE SIMMC3-TIPOID-DESTI
                                       TO TMNFWS04-CDSTNA-MSGEM
                  MOVE SIMMC3-ID-DESTI TO WRK-9-8
                  MOVE WRK-X-8         TO TMNFWS04-CSIST-PGTO-DSTNA
                  MOVE SIMMC3-NUMSEQ   TO TMNFWS04-CNRO-SEQ-BACEN
                  MOVE SIMMC3-INDCONT  TO TMNFWS04-CINDCD-CONTINUA
                  MOVE SIMMC3-NUOP     TO TMNFWS04-CNRO-UNIC-OPER

                  STRING SIMMC3-CODMSG
                         SIMMC3-NUMCTRLIF
                         SIMMC3-ISPBIF
                         SIMMC3-ISPBIFCERTIF
                         SIMMC3-DTMOVTO
                         DELIMITED BY SIZE INTO TMNFWS05-MSG

                  MOVE 'SPB02'         TO WRK-ENT-DOM-SIST

               WHEN 'GEN0018  '
                  MOVE TMNFWE05-SIMMD0 TO SIMMD0-REGISTRO
                  MOVE SIMMD0-ID-EMISSOR
                                       TO WRK-CEMISR-MSGEM-N
                  MOVE SIMMD0-ID-DESTI TO WRK-CDSTNA-MSGEM-N
                  MOVE ALL '*'         TO WRK-CTRLIF

               WHEN OTHER
                  MOVE TMNFWE05-SIMM04 TO TMNFWS04-BLOCO-BCMSG
                  MOVE TMNFWE16-CDOM-MSGRA-SPB
                                       TO WRK-ENT-DOM-SIST
                  MOVE TMNFWE05-SIMM05 TO TMNFWS05-MSG

                  MOVE TMNFWE05-SIMM02 TO WRK-BLOCO-FINANCEIRO
                  MOVE TMNFWS02-CNRO-CTRL-INSTC
                                       TO WRK-CTRLIF
                  MOVE TMNFWS04-CSIST-PGTO-EMISR
                                       TO WRK-CEMISR-MSGEM
                  MOVE TMNFWS04-CSIST-PGTO-DSTNA
                                       TO WRK-CDSTNA-MSGEM
           END-EVALUATE.

      *----------------------------------------------------------------*
       2110-99-EXIT.                   EXIT.
      *----------------------------------------------------------------*
      *----------------------------------------------------------------*
       2120-GERAR-NUM-TMNF             SECTION.
      *----------------------------------------------------------------*

           MOVE CORR WRK-TSTAMP        TO  WRK-NUM-UNICO-TMNF

           EXEC SQL
                SET :WRK-SEQUENCIA =
                     NEXT VALUE FOR SEQ_TMNFN001_TMNTRC_MSGRA
           END-EXEC

           IF (SQLCODE                 NOT EQUAL ZEROS
           OR  SQLWARN0                EQUAL 'W')
               INITIALIZE ERRO-AREA
               MOVE 'SEQ_TMNFN001'     TO ERR-DBD-TAB
               MOVE 'NEXT VALUE'       TO ERR-FUN-COMANDO
               MOVE '0010'             TO ERR-LOCAL
               MOVE 'S'                TO WRK-EXECUTA-ROLLBACK
               PERFORM 9200-TRATAR-ERRO-DB2
               PERFORM 3300-PROCEDIMENTOS-FINAIS
           END-IF.

           MOVE WRK-SEQUENCIA          TO WRK-SEQ-TIRA-SINAL.
           MOVE WRK-SEQ-SEM-SINAL      TO WRK-TSTAMP-SEQ.

      *----------------------------------------------------------------*
       2120-99-EXIT.                   EXIT.
      *----------------------------------------------------------------*
      *----------------------------------------------------------------*
       2140-GERAR-NUM-OP               SECTION.
      *----------------------------------------------------------------*

           IF TMNFWS04-CNRO-SEQ-BACEN  NOT GREATER 1
              PERFORM 2141-GERA-NUOP
           ELSE
080318        PERFORM 2144-OBTER-NUOP-NOVA-ESTRT
           END-IF.

      *----------------------------------------------------------------*
       2140-99-EXIT.                   EXIT.
      *----------------------------------------------------------------*
      *----------------------------------------------------------------*
       2141-GERA-NUOP                  SECTION.
      *----------------------------------------------------------------*

           MOVE WRK-CEMISR-MSGEM       TO WRK-NUOP-ISPB
                                          WRK-NUOP-ISPB-SPB02
           IF TMNFWE16-CDOM-MSGRA-SPB  NOT EQUAL 'SPB02'
              MOVE WRK-TSTAMP-DATA     OF WRK-NUM-UNICO-TMNF
                                       TO WRK-NUOP-DATA
              EXEC SQL
                 SET :WRK-SEQUEN-NUOP =
                     NEXT VALUE FOR DB2PRD.SEQ_TMNFN002_TMNTRC_MSGRA
              END-EXEC

           ELSE
              MOVE WRK-TSTAMP-DATA     OF WRK-NUM-UNICO-TMNF(3:6)
                                       TO WRK-NUOP-DATA-SPB02
              EXEC SQL
                   SET :WRK-SEQUEN-NUOP-SPB02 =
                        NEXT VALUE FOR DB2PRD.SEQ_TMNFN003_NOPER_SPB02
              END-EXEC
           END-IF.

           IF (SQLCODE                 NOT EQUAL ZEROS)
           OR (SQLWARN0                EQUAL 'W')
              INITIALIZE ERRO-AREA
              MOVE 'SEQ_TMNFN002'      TO ERR-DBD-TAB
              MOVE 'NEXT VALUE'        TO ERR-FUN-COMANDO
              MOVE '0015'              TO ERR-LOCAL
              PERFORM 9200-TRATAR-ERRO-DB2
              PERFORM 3300-PROCEDIMENTOS-FINAIS
           END-IF.

           IF TMNFWE16-CDOM-MSGRA-SPB  EQUAL 'SPB02'
              MOVE WRK-SEQUEN-NUOP-SPB02
                                       TO WRK-NUOP-TIRA-SINAL-SPB02
              MOVE WRK-NUOP-SEM-SINAL-SPB02
                                       TO WRK-NUOP-SEQ-SPB02
              MOVE WRK-NUOP-SPB02      TO WRK-NUOP
           ELSE
              MOVE WRK-SEQUEN-NUOP     TO WRK-NUOP-TIRA-SINAL
              MOVE WRK-NUOP-SEM-SINAL
                                       TO WRK-NUOP-SEQ
              MOVE WRK-NUOP-OUTROS     TO WRK-NUOP
           END-IF.

      *----------------------------------------------------------------*
       2141-99-FIM.                    EXIT.
      *----------------------------------------------------------------*
      *----------------------------------------------------------------*
       2143-ETAPA-GERAR-NUOP           SECTION.
      *----------------------------------------------------------------*
AJG        MOVE WRK-TMNFB029-NREG      TO WRK-TMNFB029-NREG-AUX.
AJG        ADD 1                       TO WRK-TMNFB029-NREG-AUX.
AJG        MOVE WRK-TMNFB029-NREG-AUX  TO WRK-TMNFB029-NREG.
           MOVE WRK-FASE               TO WRK-CFASE-PROCM
                                         (WRK-TMNFB029-NREG)
           MOVE WRK-SIT-FASE           TO WRK-SIT-FASE-B29
                                         (WRK-TMNFB029-NREG)
           MOVE WRK-TIMESTAMP          TO WRK-HINIC-MNTRC
                                         (WRK-TMNFB029-NREG)
           PERFORM 1200-OBTER-DATA-HORA.
           MOVE WRK-TIMESTAMP          TO WRK-HFNAL-MNTRC
                                         (WRK-TMNFB029-NREG)
           MOVE ZEROS                  TO WRK-NULL-RERRO-ETAPA-MNTRC
           MOVE WRK-DESC-WORKF         TO WRK-RERRO-MNTRC
                                         (WRK-TMNFB029-NREG).
           SET WRK-ERR-PROCESSO        TO TRUE.
           MOVE 'S'                    TO WRK-MSG-PROBLEMA.

      *----------------------------------------------------------------*
       2143-99-FIM.                    EXIT.
      *----------------------------------------------------------------*
      *----------------------------------------------------------------*
       2144-OBTER-NUOP-NOVA-ESTRT      SECTION.
      *----------------------------------------------------------------*

           EVALUATE TRUE
              WHEN WRK-DOMINGO
                 PERFORM 2144-01-OBTER-NUOP-DOM

              WHEN WRK-SEGUNDA
                 PERFORM 2144-02-OBTER-NUOP-SEG

              WHEN WRK-TERCA
                 PERFORM 2144-03-OBTER-NUOP-TER

              WHEN WRK-QUARTA
                 PERFORM 2144-04-OBTER-NUOP-QUA

              WHEN WRK-QUINTA
                 PERFORM 2144-05-OBTER-NUOP-QUI

              WHEN WRK-SEXTA
                 PERFORM 2144-06-OBTER-NUOP-SEX

              WHEN WRK-SABADO
                 PERFORM 2144-07-OBTER-NUOP-SAB

           END-EVALUATE.

           IF SQLCODE                  NOT EQUAL ZEROS
              PERFORM 2144-08-OBTER-NUOP-CONTG
           END-IF.

      *----------------------------------------------------------------*
       2144-99-FIM.                    EXIT.
      *----------------------------------------------------------------*
      *----------------------------------------------------------------*
       2144-01-OBTER-NUOP-DOM          SECTION.
      *----------------------------------------------------------------*
           MOVE TMNFWS02-CNRO-CTRL-INSTC
                                       TO WRK-HOST-CCTRL-INSTC.

           EXEC SQL
              SELECT VALUE (CUNIC_TRFGO_MSGEM, ' ')
                INTO :WRK-NUOP
                FROM DB2PRD.TMNTRC_MSGRA_DMNGO
               WHERE CCTRL_RQUIS_INSTC = :WRK-HOST-CCTRL-INSTC
                 AND (CINDCD_ORIGE_TRFGO
                                       = :WRK-ENVIO-LEGADO
                  OR  CINDCD_ORIGE_TRFGO
                                       = :WRK-ENVIO-MANUAL)
               FETCH FIRST ROW ONLY
                WITH UR
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS AND +100)
           OR (SQLWARN0                EQUAL   'W')
              MOVE 'TMNTRC_MSGRA_DMNGO'
                                      TO ERR-DBD-TAB
              MOVE 'SELECT'           TO ERR-FUN-COMANDO
              MOVE '0025'             TO ERR-LOCAL
              PERFORM 9200-TRATAR-ERRO-DB2
              PERFORM 3300-PROCEDIMENTOS-FINAIS
           END-IF.

      *----------------------------------------------------------------*
       2144-01-99-FIM.                 EXIT.
      *----------------------------------------------------------------*
      *----------------------------------------------------------------*
       2144-02-OBTER-NUOP-SEG          SECTION.
      *----------------------------------------------------------------*
           MOVE TMNFWS02-CNRO-CTRL-INSTC
                                       TO WRK-HOST-CCTRL-INSTC

           EXEC SQL
              SELECT VALUE (CUNIC_TRFGO_MSGEM, ' ')
                INTO :WRK-NUOP
                FROM DB2PRD.TMNTRC_MSGRA_SEGDA
               WHERE CCTRL_RQUIS_INSTC = :WRK-HOST-CCTRL-INSTC
                 AND (CINDCD_ORIGE_TRFGO
                                       = :WRK-ENVIO-LEGADO
                  OR  CINDCD_ORIGE_TRFGO
                                       = :WRK-ENVIO-MANUAL)
               FETCH FIRST ROW ONLY
                WITH UR
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS AND +100)
           OR (SQLWARN0                EQUAL   'W')
              MOVE 'TMNTRC_MSGRA_SEGDA'
                                      TO ERR-DBD-TAB
              MOVE 'SELECT'           TO ERR-FUN-COMANDO
              MOVE '0030'             TO ERR-LOCAL
              PERFORM 9200-TRATAR-ERRO-DB2
              PERFORM 3300-PROCEDIMENTOS-FINAIS
           END-IF.

      *----------------------------------------------------------------*
       2144-02-99-FIM.                 EXIT.
      *----------------------------------------------------------------*
      *----------------------------------------------------------------*
       2144-03-OBTER-NUOP-TER          SECTION.
      *----------------------------------------------------------------*
           MOVE TMNFWS02-CNRO-CTRL-INSTC
                                       TO WRK-HOST-CCTRL-INSTC.

           EXEC SQL
              SELECT VALUE (CUNIC_TRFGO_MSGEM, ' ')
                INTO :WRK-NUOP
                FROM DB2PRD.TMNTRC_MSGRA_TRC
               WHERE CCTRL_RQUIS_INSTC = :WRK-HOST-CCTRL-INSTC
                 AND (CINDCD_ORIGE_TRFGO
                                       = :WRK-ENVIO-LEGADO
                  OR  CINDCD_ORIGE_TRFGO
                                       = :WRK-ENVIO-MANUAL)
               FETCH FIRST ROW ONLY
                WITH UR
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS AND +100)
           OR (SQLWARN0                EQUAL   'W')
              MOVE 'TMNTRC_MSGRA_TRC'  TO ERR-DBD-TAB

              MOVE 'SELECT'            TO ERR-FUN-COMANDO
              MOVE '0045'              TO ERR-LOCAL
              PERFORM 9200-TRATAR-ERRO-DB2
              PERFORM 3300-PROCEDIMENTOS-FINAIS
           END-IF.

      *----------------------------------------------------------------*
       2144-03-99-FIM.                 EXIT.
      *----------------------------------------------------------------*
      *----------------------------------------------------------------*
       2144-04-OBTER-NUOP-QUA          SECTION.
      *----------------------------------------------------------------*
           MOVE TMNFWS02-CNRO-CTRL-INSTC
                                       TO WRK-HOST-CCTRL-INSTC

           EXEC SQL
              SELECT VALUE (CUNIC_TRFGO_MSGEM, ' ')
                INTO :WRK-NUOP
                FROM DB2PRD.TMNTRC_MSGRA_QUART
               WHERE CCTRL_RQUIS_INSTC = :WRK-HOST-CCTRL-INSTC
                 AND (CINDCD_ORIGE_TRFGO
                                       = :WRK-ENVIO-LEGADO
                  OR  CINDCD_ORIGE_TRFGO
                                       = :WRK-ENVIO-MANUAL)
               FETCH FIRST ROW ONLY
                WITH UR
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS AND +100)
           OR (SQLWARN0                EQUAL   'W')
              MOVE 'TMNTRC_MSGRA_QUART'  TO ERR-DBD-TAB

              MOVE 'SELECT'            TO ERR-FUN-COMANDO
              MOVE '0050'              TO ERR-LOCAL
              PERFORM 9200-TRATAR-ERRO-DB2
              PERFORM 3300-PROCEDIMENTOS-FINAIS
           END-IF.

      *----------------------------------------------------------------*
       2144-04-99-FIM.                 EXIT.
      *----------------------------------------------------------------*
      *----------------------------------------------------------------*
       2144-05-OBTER-NUOP-QUI          SECTION.
      *----------------------------------------------------------------*
           MOVE TMNFWS02-CNRO-CTRL-INSTC
                                       TO WRK-HOST-CCTRL-INSTC

           EXEC SQL
              SELECT VALUE (CUNIC_TRFGO_MSGEM, ' ')
                INTO :WRK-NUOP
                FROM DB2PRD.TMNTRC_MSGRA_QTA
               WHERE CCTRL_RQUIS_INSTC = :WRK-HOST-CCTRL-INSTC
                 AND (CINDCD_ORIGE_TRFGO
                                       = :WRK-ENVIO-LEGADO
                  OR  CINDCD_ORIGE_TRFGO
                                       = :WRK-ENVIO-MANUAL)
               FETCH FIRST ROW ONLY
                WITH UR
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS AND +100)
           OR (SQLWARN0                EQUAL   'W')
              MOVE 'TMNTRC_MSGRA_QTA'  TO ERR-DBD-TAB
              MOVE 'SELECT'            TO ERR-FUN-COMANDO
              MOVE '0065'              TO ERR-LOCAL
              PERFORM 9200-TRATAR-ERRO-DB2
              PERFORM 3300-PROCEDIMENTOS-FINAIS
           END-IF.

      *----------------------------------------------------------------*
       2144-05-99-FIM.                 EXIT.
      *----------------------------------------------------------------*
      *----------------------------------------------------------------*
       2144-06-OBTER-NUOP-SEX          SECTION.
      *----------------------------------------------------------------*
           MOVE TMNFWS02-CNRO-CTRL-INSTC
                                       TO WRK-HOST-CCTRL-INSTC

           EXEC SQL
              SELECT VALUE (CUNIC_TRFGO_MSGEM, ' ')
                INTO :WRK-NUOP
                FROM DB2PRD.TMNTRC_MSGRA_SEXTA
               WHERE CCTRL_RQUIS_INSTC = :WRK-HOST-CCTRL-INSTC
                 AND (CINDCD_ORIGE_TRFGO
                                       = :WRK-ENVIO-LEGADO
                  OR  CINDCD_ORIGE_TRFGO
                                       = :WRK-ENVIO-MANUAL)
               FETCH FIRST ROW ONLY
                WITH UR
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS AND +100)
           OR (SQLWARN0                EQUAL   'W')
              MOVE 'TMNTRC_MSGRA_SEXTA'
                                       TO ERR-DBD-TAB
              MOVE 'SELECT'            TO ERR-FUN-COMANDO
              MOVE '0070'              TO ERR-LOCAL
              PERFORM 9200-TRATAR-ERRO-DB2
              PERFORM 3300-PROCEDIMENTOS-FINAIS
           END-IF.

      *----------------------------------------------------------------*
       2144-06-99-FIM.                 EXIT.
      *----------------------------------------------------------------*
      *----------------------------------------------------------------*
       2144-07-OBTER-NUOP-SAB          SECTION.
      *----------------------------------------------------------------*
           MOVE TMNFWS02-CNRO-CTRL-INSTC
                                       TO WRK-HOST-CCTRL-INSTC

           EXEC SQL
              SELECT VALUE (CUNIC_TRFGO_MSGEM, ' ')
                INTO :WRK-NUOP
                FROM DB2PRD.TMNTRC_MSGRA_SBADO
               WHERE CCTRL_RQUIS_INSTC = :WRK-HOST-CCTRL-INSTC
                 AND (CINDCD_ORIGE_TRFGO
                                       = :WRK-ENVIO-LEGADO
                  OR  CINDCD_ORIGE_TRFGO
                                       = :WRK-ENVIO-MANUAL)
               FETCH FIRST ROW ONLY
                WITH UR
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS AND +100)
           OR (SQLWARN0                EQUAL   'W')
              MOVE 'TMNTRC_MSGRA_SEXTA'
                                       TO ERR-DBD-TAB
              MOVE 'SELECT'            TO ERR-FUN-COMANDO
              MOVE '0075'              TO ERR-LOCAL
              PERFORM 9200-TRATAR-ERRO-DB2
              PERFORM 3300-PROCEDIMENTOS-FINAIS
           END-IF.

      *----------------------------------------------------------------*
       2144-07-99-FIM.                 EXIT.
      *----------------------------------------------------------------*
      *----------------------------------------------------------------*
       2144-08-OBTER-NUOP-CONTG        SECTION.
      *----------------------------------------------------------------*
           MOVE TMNFWS02-CNRO-CTRL-INSTC
                                       TO WRK-HOST-CCTRL-INSTC

           EXEC SQL
              SELECT VALUE (CUNIC_TRFGO_MSGEM, ' ')
                INTO :WRK-NUOP
                FROM DB2PRD.TMNTRC_MSGRA_CONTG
               WHERE CCTRL_RQUIS_INSTC = :WRK-HOST-CCTRL-INSTC
                 AND (CINDCD_ORIGE_TRFGO
                                       = :WRK-ENVIO-LEGADO
                  OR  CINDCD_ORIGE_TRFGO
                                       = :WRK-ENVIO-MANUAL)
               FETCH FIRST ROW ONLY
                WITH UR
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS AND +100)
           OR (SQLWARN0                EQUAL   'W')
              MOVE 'TMNTRC_MSGRA_CONTG'
                                       TO ERR-DBD-TAB
              MOVE 'SELECT'            TO ERR-FUN-COMANDO
              MOVE '0080'              TO ERR-LOCAL
              PERFORM 9200-TRATAR-ERRO-DB2
              PERFORM 3300-PROCEDIMENTOS-FINAIS
           END-IF.

           IF SQLCODE                  NOT EQUAL ZEROS
              MOVE 2                   TO ERR-RETURN-CODE
              MOVE LOW-VALUES          TO WRK-NUOP
              MOVE 63                  TO WRK-FASE
              MOVE 5                   TO WRK-SIT-FASE
              INITIALIZE WRK-DESC-WORKF
              MOVE 'NUOP NAO ENCONTRADA PARA MSG COM SEQ'
                                       TO WRK-DESC-WORKF-LN1
              MOVE WRK-DESC-WORKF-LN1  TO ERR-TEXTO-MSG
              PERFORM 2143-ETAPA-GERAR-NUOP
              PERFORM 3000-FINALIZAR
           END-IF.

      *----------------------------------------------------------------*
       2144-08-99-FIM.                 EXIT.
      *----------------------------------------------------------------*
      *----------------------------------------------------------------*
       2200-CONVERSAO-TXT-XML          SECTION.
      *----------------------------------------------------------------*
           MOVE '2200-CONVERSAO-TXT-XML'
                                       TO WRK-PARAGRAFO

      *----------------------------------------------------------------*
      * MODULO QUE RECEBE TXT NO PADRAO DA MENSAGERIA E CONVERTE
      * EM XML.
      * ESSE MODULO TAMBEM RETORNARA A ESTUTURA E A VERSAO QUE SERAO
      * UTILIZADAS NA MONITORACAO.
      *----------------------------------------------------------------*

           INITIALIZE WRK-AREA-TMNF5040.

           MOVE WRK-TMNF5026-ENTRADA   TO WRK-AREA-TMNF5040
051217     MOVE TMNFWS05-MSG(1:9)      TO WRK-MSG
AJG12      MOVE TMNFWS04-CSIST-PGTO-EMISR
AJG12                                  TO WRK-CEMISR-MSGEM
AJG12      MOVE TMNFWS04-CSIST-PGTO-DSTNA
AJG12                                  TO WRK-CDSTNA-MSGEM

           EXEC CICS LINK
               PROGRAM (WRK-TMNF5040)
               COMMAREA(WRK-AREA-TMNF5040)
               LENGTH  (LENGTH OF WRK-AREA-TMNF5040)
               NOHANDLE
           END-EXEC.

           IF (EIBRESP                 NOT EQUAL DFHRESP(NORMAL))
               INITIALIZE ERRO-AREA
               MOVE WRK-TMNF5040       TO ERR-MODULO
               MOVE '0085'             TO ERR-LOCAL
               PERFORM 9300-TRATAR-ERRO-CICS
               PERFORM 3000-FINALIZAR
           END-IF.

           MOVE WRK-AREA-TMNF5040(1:LENGTH
                                       OF WRK-AREA-TMNF5040)
                                       TO WRK-AREA-SAIDA-TMNF5040
                                         (1:LENGTH OF
                                            WRK-AREA-TMNF5040).

           IF TMNFW820-COD-RETORNO     NOT EQUAL ZEROS
              MOVE 30                  TO WRK-FASE
              MOVE 5                   TO WRK-SIT-FASE
              PERFORM 2210-ETAPA-CONV-TXT-XML

              IF TMNFW820-COD-RETORNO  EQUAL 12
                 MOVE 'S'           TO WRK-MSG-PROBLEMA
                 MOVE LOW-VALUES    TO WRK-NUOP
              END-IF

              MOVE WRK-DESC-WORKF-LN1  TO ERR-TEXTO-MSG
              PERFORM 3000-FINALIZAR
           ELSE
              MOVE 29                  TO WRK-FASE
              MOVE 2                   TO WRK-SIT-FASE
              PERFORM 2210-ETAPA-CONV-TXT-XML
           END-IF.

      *----------------------------------------------------------------*
       2200-99-EXIT.                   EXIT.
      *----------------------------------------------------------------*
      *----------------------------------------------------------------*
       2210-ETAPA-CONV-TXT-XML         SECTION.
      *----------------------------------------------------------------*
AJG        MOVE WRK-TMNFB029-NREG      TO WRK-TMNFB029-NREG-AUX.
AJG        ADD 1                       TO WRK-TMNFB029-NREG-AUX.
AJG        MOVE WRK-TMNFB029-NREG-AUX  TO WRK-TMNFB029-NREG.


           MOVE WRK-FASE               TO WRK-CFASE-PROCM
                                         (WRK-TMNFB029-NREG)

           MOVE WRK-SIT-FASE           TO WRK-SIT-FASE-B29
                                         (WRK-TMNFB029-NREG)

           MOVE ZEROS                  TO WRK-NULL-RERRO-ETAPA-MNTRC
           MOVE TMNFW820-DESC-MENSAGEM TO WRK-RERRO-MNTRC
                                         (WRK-TMNFB029-NREG).

           MOVE TMNFW820-DESC-MENSAGEM(1:LENGTH OF WRK-DESC-WORKF)
                                       TO WRK-DESC-WORKF.

           MOVE WRK-TIMESTAMP          TO WRK-HINIC-MNTRC
                                         (WRK-TMNFB029-NREG).
           PERFORM 1200-OBTER-DATA-HORA.
           MOVE WRK-TIMESTAMP          TO WRK-HFNAL-MNTRC
                                         (WRK-TMNFB029-NREG).

           IF WRK-SIT-FASE             NOT EQUAL 2
              MOVE 2                   TO ERR-RETURN-CODE
              MOVE ZEROS               TO TMNFW820-S-TAM-XML
              SET WRK-ERR-RECB-LEGADO  TO TRUE
              INITIALIZE TMNFWS03-BL-RET-BDSCO
              MOVE 1001                TO TMNFWS03-CRETOR-PRINC-N
              MOVE 99                  TO TMNFWS03-CRETOR-OPCAO-N
              MOVE 'ERRO NA CONVERSAO TEXTO XML: '
                                       TO TMNFWS03-RMSGEM-ERRO-X
              MOVE TMNFW820-DESC-MENSAGEM(1:40)
                                       TO TMNFWS03-RMSGEM-ERRO-X
                                          (40:40)
              PERFORM 5000-INF-BLOCO-RETORNO

              MOVE 2                   TO WRK-CINDCD-RPROC-AUX
              MOVE 'N'                 TO WRK-MSG-PROBLEMA
REPROC        MOVE 'S'                 TO WRK-RPROC-CONVERSAO
           END-IF.

      *----------------------------------------------------------------*
       2210-99-EXIT.                   EXIT.
      *----------------------------------------------------------------*
      *----------------------------------------------------------------*
       2600-TRATAR-GRADE-HORARIA       SECTION.
      *----------------------------------------------------------------*

           MOVE 'S'                    TO WRK-TRATA-GRADE.

           PERFORM 2610-OBTEM-GRADE.

           IF WRK-TRATA-GRADE          NOT EQUAL 'S'
              GO                       TO 2600-99-FIM
           END-IF.

           INITIALIZE WRK-DESC-WORKF.

           IF (HABERT-GRADE-SIST       OF TMNFB034
                                       NOT EQUAL WRK-TIMESTAMP-ZEROS
           AND HABERT-GRADE-SIST       OF TMNFB034
                                       GREATER TMNFWE16-HOCORR-WKFLW)
           OR (HFCHTO-GRADE-SIST       OF TMNFB034
                                       NOT EQUAL WRK-TIMESTAMP-ZEROS
           AND HFCHTO-GRADE-SIST       OF TMNFB034
                                       LESS TMNFWE16-HOCORR-WKFLW)
               MOVE 'FORA DA GRADE DE HORARIO'
                                       TO WRK-DESC-WORKF-LN1
               PERFORM 2620-GRADE-FECHADA
           END-IF.

           IF  HABERT-GRADE-EVTAL      OF TMNFB034
                                       NOT EQUAL WRK-TIMESTAMP-ZEROS
           AND HFCHTO-GRADE-EVTAL      OF TMNFB034
                                       NOT EQUAL WRK-TIMESTAMP-ZEROS
               IF (TMNFWE16-HOCORR-WKFLW
                                       LESS HABERT-GRADE-EVTAL
                                       OF  TMNFB034)
               OR (TMNFWE16-HOCORR-WKFLW
                                       GREATER HFCHTO-GRADE-EVTAL
                                       OF  TMNFB034)
                   MOVE 'FORA DA GRADE DE HORARIO EVENTUAL'
                                       TO WRK-DESC-WORKF-LN1
                   PERFORM 2620-GRADE-FECHADA
               END-IF
           ELSE
               IF (TMNFWE16-HOCORR-WKFLW
                                       LESS HABERT-GRADE-PDRAO
                                       OF  TMNFB034)
                   MOVE 'FORA DA GRADE DE HORARIO PADRAO'
                                       TO WRK-DESC-WORKF-LN1
                   PERFORM 2620-GRADE-FECHADA
               END-IF

               IF  (HFCHTO-GRADE-PDRAO OF TMNFB034
                                       NOT EQUAL WRK-TIMESTAMP-ZEROS)
               AND (TMNFWE16-HOCORR-WKFLW
                                       GREATER HFCHTO-GRADE-PDRAO
                                       OF  TMNFB034)
                   MOVE 'FORA DA GRADE DE HORARIO PADRAO'
                                       TO WRK-DESC-WORKF-LN1
                   PERFORM 2620-GRADE-FECHADA
               END-IF
           END-IF.

      *----------------------------------------------------------------*
       2600-99-FIM.                    EXIT.
      *----------------------------------------------------------------*
      *----------------------------------------------------------------*
       2610-OBTEM-GRADE                SECTION.
      *----------------------------------------------------------------*
           INITIALIZE TMNFB036
                      TMNFB034.

           MOVE TMNFW820-S-NESTRT-INFO TO NESTRT-INFO-BACEN
                                       OF TMNFB036

           MOVE WRK-CDSTNA-MSGEM       TO CISPB-PARTC-SIST
                                       OF TMNFB036

           MOVE TMNFWE16-HOCORR-WKFLW(1:10)
                                       TO HREFT-GRADE
                                       OF TMNFB034

           EXEC SQL
              SELECT VALUE(A.CGRADE_INSTC_FINCR, ' ')
                   , VALUE(B.HREFT_GRADE,
                                 '0001-01-01-00.00.00.000000')
                   , VALUE(B.HABERT_GRADE_PDRAO,
                                 '0001-01-01-00.00.00.000000')
                   , VALUE(B.HFCHTO_GRADE_PDRAO,
                                 '0001-01-01-00.00.00.000000')
                   , VALUE(B.HABERT_GRADE_EVTAL,
                                 '0001-01-01-00.00.00.000000')
                   , VALUE(B.HFCHTO_GRADE_EVTAL,
                                 '0001-01-01-00.00.00.000000')
                   , VALUE(B.HABERT_GRADE_SIST,
                                 '0001-01-01-00.00.00.000000')
                   , VALUE(B.HFCHTO_GRADE_SIST,
                                 '0001-01-01-00.00.00.000000')
                INTO :TMNFB036.CGRADE-INSTC-FINCR
                   , :TMNFB034.HREFT-GRADE
                   , :TMNFB034.HABERT-GRADE-PDRAO
                   , :TMNFB034.HFCHTO-GRADE-PDRAO
                   , :TMNFB034.HABERT-GRADE-EVTAL
                   , :TMNFB034.HFCHTO-GRADE-EVTAL
                   , :TMNFB034.HABERT-GRADE-SIST
                   , :TMNFB034.HFCHTO-GRADE-SIST
                FROM DB2PRD.TGRADE_INSTC_MSGEM A
                   , DB2PRD.TGRADE_HORA_INSTC B
               WHERE A.NESTRT_INFO_BACEN = :TMNFB036.NESTRT-INFO-BACEN
                 AND A.CISPB_PARTC_SIST  = :TMNFB036.CISPB-PARTC-SIST
                 AND A.CGRADE_INSTC_FINCR
                                         = B.CGRADE_INSTC_FINCR
                 AND A.CISPB_PARTC_SIST  = B.CISPB_PARTC_SIST
                 AND DATE(B.HREFT_GRADE) = DATE(:TMNFB034.HREFT-GRADE)
               FETCH FIRST ROW ONLY
                WITH UR
           END-EXEC

           IF (SQLCODE                 NOT EQUAL ZEROS AND +100)
           OR (SQLWARN0                EQUAL   'W')
               INITIALIZE ERRO-AREA
               MOVE 'TGRADE_HORA_INSTC'
                                       TO ERR-DBD-TAB
               MOVE 'SELECT'           TO ERR-FUN-COMANDO
               MOVE '0135'             TO ERR-LOCAL
               PERFORM 9200-TRATAR-ERRO-DB2
               PERFORM 3000-FINALIZAR
           END-IF.

           IF SQLCODE                  EQUAL +100
              MOVE 'N'                 TO WRK-TRATA-GRADE
           END-IF.

      *----------------------------------------------------------------*
       2610-99-EXIT.                   EXIT.
      *----------------------------------------------------------------*
      *----------------------------------------------------------------*
       2620-GRADE-FECHADA              SECTION.
      *----------------------------------------------------------------*

           MOVE 67                     TO WRK-FASE
           PERFORM 2630-ETAPA-GRADE-HORARIA.

           MOVE 2                      TO ERR-RETURN-CODE.
           MOVE WRK-DESC-WORKF-LN1     TO ERR-TEXTO-MSG.
           PERFORM 3000-FINALIZAR.

      *----------------------------------------------------------------*
       2620-99-EXIT.                   EXIT.
      *----------------------------------------------------------------*
      *----------------------------------------------------------------*
       2630-ETAPA-GRADE-HORARIA        SECTION.
      *----------------------------------------------------------------*

AJG        MOVE WRK-TMNFB029-NREG      TO WRK-TMNFB029-NREG-AUX.
AJG        ADD 1                       TO WRK-TMNFB029-NREG-AUX.
AJG        MOVE WRK-TMNFB029-NREG-AUX  TO WRK-TMNFB029-NREG.

           MOVE WRK-FASE               TO WRK-CFASE-PROCM
                                         (WRK-TMNFB029-NREG).

           MOVE 5                      TO WRK-SIT-FASE-B29
                                         (WRK-TMNFB029-NREG).

           MOVE WRK-DESC-WORKF         TO WRK-RERRO-MNTRC
                                         (WRK-TMNFB029-NREG).

           MOVE WRK-TIMESTAMP          TO WRK-HINIC-MNTRC
                                         (WRK-TMNFB029-NREG).

           PERFORM 1200-OBTER-DATA-HORA.
           MOVE WRK-TIMESTAMP          TO WRK-HFNAL-MNTRC
                                         (WRK-TMNFB029-NREG).

           SET WRK-ERR-PROCESSO        TO TRUE.

           INITIALIZE TMNFWS03-BL-RET-BDSCO.
           MOVE 1000                   TO TMNFWS03-CRETOR-PRINC-N
           MOVE 86                     TO TMNFWS03-CRETOR-OPCAO-N
           MOVE 'MENSAGEM RECUSADA: '  TO TMNFWS03-RMSGEM-ERRO-X
           MOVE WRK-DESC-WORKF(1:60)   TO TMNFWS03-RMSGEM-ERRO-X
                                          (20:60).
           PERFORM 5000-INF-BLOCO-RETORNO.

           MOVE 'N'                    TO WRK-MSG-PROBLEMA.

      *----------------------------------------------------------------*
       2630-99-EXIT.                   EXIT.
      *----------------------------------------------------------------*
      *----------------------------------------------------------------*
       2700-TRATAR-CRIPTO              SECTION.
      *----------------------------------------------------------------*
           MOVE '2700-TRATAR-CRIPTO'   TO WRK-PARAGRAFO

           INITIALIZE WRK-AREA-SCRY301O

           MOVE '1'                    TO LNK-301-MCA-FORMATO.
           MOVE 'CRIPTOGRAFAR   '      TO LNK-301-MCA-FUNCAO-CIFRAGEM.
           MOVE WRK-CEMISR-MSGEM       TO LNK-301-MCA-ISPB-ORIG
           MOVE WRK-CDSTNA-MSGEM       TO LNK-301-MCA-ISPB-DEST
           MOVE ZEROS                  TO LNK-301-MCA-CODIGO-GEN.

           IF WRK-ENT-DOM-SIST(1:3)    EQUAL 'MES'
              MOVE 02                  TO LNK-301-MCA-DOMINIO-R
           ELSE
              MOVE 01                  TO LNK-301-MCA-DOMINIO-R
           END-IF

           MOVE TMNFW820-S-TAM-XML     TO LNK-301-LEN-AREA.
           MOVE FUNCTION NATIONAL-OF
                        (TMNFW820-S-XML(1:TMNFW820-S-TAM-XML))
                                       TO WRK-NATIONAL

CAT412     IF  LNK-301-MCA-DOMINIO-R   EQUAL 01
.              SET WRK-CCSID-UTF-8     TO TRUE
.              MOVE FUNCTION DISPLAY-OF(WRK-NATIONAL, WRK-CCSID)
.                                      TO LNK-301-AREA
.          ELSE
.     *       XSET WRK-CCSID-UTF-16    TO TRUE
.              MOVE WRK-NATIONAL-X     TO LNK-301-AREA
.              COMPUTE LNK-301-LEN-AREA
.                                      = LNK-301-LEN-AREA
.                                      * 2
CAT412     END-IF.

           SET WRK-AREA-SCRY301O-END   TO ADDRESS
                                       OF WRK-AREA-SCRY301O

           EXEC CICS LINK
               PROGRAM  (WRK-SCRY301O)
               COMMAREA (WRK-AREA-SCRY301O-END)
               LENGTH   (LENGTH OF WRK-AREA-SCRY301O-END)
               NOHANDLE
           END-EXEC.

           IF  EIBRESP                 NOT EQUAL DFHRESP(NORMAL)
               MOVE WRK-SCRY301O       TO ERR-MODULO
               MOVE '0140'             TO ERR-LOCAL
               PERFORM 9300-TRATAR-ERRO-CICS
               PERFORM 3000-FINALIZAR
           END-IF

           IF  LNK-301-MCA-RETURN-CODE NOT EQUAL ZEROS
               INITIALIZE WRK-DESC-WORKF
               MOVE 2                  TO ERR-RETURN-CODE
               MOVE 5                  TO WRK-SIT-FASE
               MOVE 32                 TO WRK-FASE
               MOVE 'ERRO AO CRIPTOGRAFAR MENSAGEM.'
                                       TO WRK-DESC-WORKF-LN1
                                          ERR-TEXTO-MSG
               MOVE 'DETALHE: '        TO WRK-DESC-WORKF-LN2
               MOVE LNK-301-MSG-ERROR(1:69)
                                       TO WRK-DESC-WORKF-LN2(10:69)
               MOVE LNK-301-MSG-ERROR(70:16)
                                       TO WRK-DESC-WORKF-LN3
               PERFORM 2710-TRATAR-FASE-CRIPTO
               PERFORM 3000-FINALIZAR
           END-IF.

           MOVE 31                     TO WRK-FASE
           MOVE 2                      TO WRK-SIT-FASE
           INITIALIZE WRK-DESC-WORKF
           MOVE 'CRIPTOGRAFIA EFETUADA COM SUCESSO.'
                                       TO WRK-DESC-WORKF-LN1
           MOVE 'CERTIFICADO ORIGEM.: '
                                       TO WRK-DESC-WORKF-LN2
           MOVE LNK-CRIPTO-ORIGEM-SERIAL
181218                                 OF WRK-AREA-SCRY301O
                                       TO WRK-DESC-WORKF-LN2(22:32)
           MOVE 'CERTIFICADO DESTINO: '
                                       TO WRK-DESC-WORKF-LN3
           MOVE LNK-CRIPTO-DESTINO-SERIAL
181218                                 OF WRK-AREA-SCRY301O
                                       TO WRK-DESC-WORKF-LN3(22:32)

           PERFORM 2710-TRATAR-FASE-CRIPTO.

      *----------------------------------------------------------------*
       2700-99-EXIT.                   EXIT.
      *----------------------------------------------------------------*
      *----------------------------------------------------------------*
       2710-TRATAR-FASE-CRIPTO         SECTION.
      *----------------------------------------------------------------*

AJG        MOVE WRK-TMNFB029-NREG      TO WRK-TMNFB029-NREG-AUX.
AJG        ADD 1                       TO WRK-TMNFB029-NREG-AUX.
AJG        MOVE WRK-TMNFB029-NREG-AUX  TO WRK-TMNFB029-NREG.

           MOVE WRK-FASE               TO WRK-CFASE-PROCM
                                         (WRK-TMNFB029-NREG)

           MOVE WRK-SIT-FASE           TO WRK-SIT-FASE-B29
                                         (WRK-TMNFB029-NREG)


           MOVE WRK-TIMESTAMP          TO WRK-HINIC-MNTRC
                                         (WRK-TMNFB029-NREG)

           PERFORM 1200-OBTER-DATA-HORA.
           MOVE WRK-TIMESTAMP          TO WRK-HFNAL-MNTRC
                                         (WRK-TMNFB029-NREG).

           MOVE WRK-DESC-WORKF         TO WRK-RERRO-MNTRC
                                         (WRK-TMNFB029-NREG)

           IF WRK-SIT-FASE             NOT EQUAL 2
              SET WRK-ERR-PROCESSO     TO TRUE
              INITIALIZE TMNFWS03-BL-RET-BDSCO
              MOVE 1000                TO TMNFWS03-CRETOR-PRINC-N
              MOVE 1                   TO TMNFWS03-CRETOR-OPCAO-N
              MOVE 'ERRO NA CRIPTOGRAFIA DA MENSAGEM'
                                       TO TMNFWS03-RMSGEM-ERRO-X
              PERFORM 5000-INF-BLOCO-RETORNO

              MOVE 1                   TO WRK-CINDCD-RPROC-AUX
REPROC        MOVE 'S'                 TO WRK-RPROC-CRIPTOGRAFIA
           END-IF.

      *----------------------------------------------------------------*
       2710-99-EXIT.                   EXIT.
      *----------------------------------------------------------------*
181218*----------------------------------------------------------------*
.      2720-TRATAR-VALOR-DISCR         SECTION.
.     *----------------------------------------------------------------*
.          MOVE '2720-TRATAR-VALOR-DISCR' TO WRK-PARAGRAFO
.
.          INITIALIZE TMNFB019.
.
.     ***  ROTA P/ ENVIO:
.          MOVE 'ENVIO_'               TO WRK-CVLR-DISCT-1
.          MOVE TMNFWE16-CODMSG        TO WRK-CVLR-DISCT-2
.          MOVE WRK-CVLR-DISCT         TO CVLR-DISCT-CARAC
.                                      OF TMNFB019.
.
.     ***  DOMINIO: '0'-ROTA SCRY-ASSEMBLER
.     ***           '1'-ROTA SCRY-CONVERTIDO - PROCESSO NOVO
.
.          EXEC SQL
.             SELECT VALUE(RVLR_DISCT_CARAC, ' ')
.               INTO :TMNFB019.RVLR-DISCT-CARAC
.               FROM DB2PRD.TVLR_DISCT_CARAC
.              WHERE CVLR_DISCT_CARAC  = :TMNFB019.CVLR-DISCT-CARAC
.              FETCH FIRST ROW ONLY
.               WITH UR
.          END-EXEC.
.
.          IF (SQLCODE                 NOT EQUAL ZEROS AND +100)
.          OR (SQLWARN0                EQUAL 'W')
.              INITIALIZE ERRO-AREA
.              MOVE 'TMNTRC_MSGRA'     TO ERR-DBD-TAB
.              MOVE 'SELECT'           TO ERR-FUN-COMANDO
.              MOVE '2720'             TO ERR-LOCAL
.              PERFORM 9200-TRATAR-ERRO-DB2
.              PERFORM 3300-PROCEDIMENTOS-FINAIS
.          END-IF.
.
.          IF SQLCODE                  EQUAL +100
.             MOVE ZEROS               TO RVLR-DISCT-CARAC
.                                      OF TMNFB019
.          END-IF.
.
.     *----------------------------------------------------------------*
.      2720-99-EXIT.                   EXIT.
.     *----------------------------------------------------------------*
.     *----------------------------------------------------------------*
.      2730-TRATAR-CRIPTO-N            SECTION.
.     *----------------------------------------------------------------*
.          MOVE '2700-TRATAR-CRIPTO-N' TO WRK-PARAGRAFO
.
.          INITIALIZE LNK-331-MCA
.          INITIALIZE LNK-331-MSG-ERROR
.          INITIALIZE LNK-CRIPTO-AREA OF WRK-AREA-SCRY331O
.          INITIALIZE LNK-331-LEN-AREA-UTIL
.          INITIALIZE LNK-331-LEN-AREA
.          INITIALIZE LNK-331-AREA-1.
.
.          MOVE '1'                    TO LNK-331-MCA-FORMATO.
.          MOVE 'CRIPTOGRAFAR   '      TO LNK-331-MCA-FUNCAO-CIFRAGEM.
.          MOVE WRK-CEMISR-MSGEM       TO LNK-331-MCA-ISPB-ORIG
.          MOVE WRK-CDSTNA-MSGEM       TO LNK-331-MCA-ISPB-DEST
.          MOVE ZEROS                  TO LNK-331-MCA-CODIGO-GEN.
.
.          IF WRK-ENT-DOM-SIST(1:3)    EQUAL 'MES'
.             MOVE 02                  TO LNK-331-MCA-DOMINIO-R
.          ELSE
.             MOVE 01                  TO LNK-331-MCA-DOMINIO-R
.          END-IF
.
.          MOVE TMNFW820-S-TAM-XML     TO LNK-331-LEN-AREA.
.          MOVE FUNCTION NATIONAL-OF
.                       (TMNFW820-S-XML(1:TMNFW820-S-TAM-XML))
.                                      TO WRK-NATIONAL
.
CAT412     IF  LNK-331-MCA-DOMINIO-R   EQUAL 01
.              SET WRK-CCSID-UTF-8     TO TRUE
.              MOVE FUNCTION DISPLAY-OF(WRK-NATIONAL, WRK-CCSID)
.                                      TO LNK-331-AREA
.          ELSE
.     *       XSET WRK-CCSID-UTF-16    TO TRUE
.              MOVE WRK-NATIONAL-X     TO LNK-331-AREA
.              COMPUTE LNK-331-LEN-AREA
.                                      = LNK-331-LEN-AREA
.                                      * 2
CAT412     END-IF.
.
.          SET WRK-AREA-SCRY331O-END   TO ADDRESS
.                                      OF WRK-AREA-SCRY331O
.
.          EXEC CICS LINK
.              PROGRAM  (WRK-SCRY331O)
.              COMMAREA (WRK-AREA-SCRY331O-END)
.              LENGTH   (LENGTH OF WRK-AREA-SCRY331O-END)
.              NOHANDLE
.          END-EXEC.
.
.          IF  EIBRESP                 NOT EQUAL DFHRESP(NORMAL)
.              MOVE WRK-SCRY331O       TO ERR-MODULO
.              MOVE '2730'             TO ERR-LOCAL
.              PERFORM 9300-TRATAR-ERRO-CICS
.              PERFORM 3000-FINALIZAR
.          END-IF
.
.          IF  LNK-331-MCA-RETURN-CODE NOT EQUAL ZEROS
.              INITIALIZE WRK-DESC-WORKF
.              MOVE 2                  TO ERR-RETURN-CODE
.              MOVE 5                  TO WRK-SIT-FASE
.              MOVE 32                 TO WRK-FASE
.              MOVE 'NR2-ERRO AO CRIPTOGRAFAR MENSAGEM.'
.                                      TO WRK-DESC-WORKF-LN1
.                                         ERR-TEXTO-MSG
.              MOVE 'DETALHE: '        TO WRK-DESC-WORKF-LN2
.              MOVE LNK-331-MSG-ERROR(1:69)
.                                      TO WRK-DESC-WORKF-LN2(10:69)
.              MOVE LNK-331-MSG-ERROR(70:16)
.                                      TO WRK-DESC-WORKF-LN3
.              PERFORM 2735-TRATAR-FASE-CRIPTO
.              PERFORM 3000-FINALIZAR
.          END-IF.
.
.          MOVE 31                     TO WRK-FASE
.          MOVE 2                      TO WRK-SIT-FASE
.          INITIALIZE WRK-DESC-WORKF
.          MOVE 'NR2-CRIPTOGRAFIA EFETUADA COM SUCESSO.'
.                                      TO WRK-DESC-WORKF-LN1
.          MOVE 'CERTIFICADO ORIGEM.: '
.                                      TO WRK-DESC-WORKF-LN2
.          MOVE LNK-CRIPTO-ORIGEM-SERIAL
.                                      OF WRK-AREA-SCRY331O
.                                      TO WRK-DESC-WORKF-LN2(22:32)
.          MOVE 'CERTIFICADO DESTINO: '
.                                      TO WRK-DESC-WORKF-LN3
.          MOVE LNK-CRIPTO-DESTINO-SERIAL
.                                      OF WRK-AREA-SCRY331O
.                                      TO WRK-DESC-WORKF-LN3(22:32)
.
.          PERFORM 2735-TRATAR-FASE-CRIPTO.
.
.     *----------------------------------------------------------------*
.      2730-99-EXIT.                   EXIT.
.     *----------------------------------------------------------------*
.     *----------------------------------------------------------------*
.      2735-TRATAR-FASE-CRIPTO         SECTION.
.     *----------------------------------------------------------------*
.
.          MOVE WRK-TMNFB029-NREG      TO WRK-TMNFB029-NREG-AUX.
.          ADD 1                       TO WRK-TMNFB029-NREG-AUX.
.          MOVE WRK-TMNFB029-NREG-AUX  TO WRK-TMNFB029-NREG.
.
.          MOVE WRK-FASE               TO WRK-CFASE-PROCM
.                                        (WRK-TMNFB029-NREG)
.
.          MOVE WRK-SIT-FASE           TO WRK-SIT-FASE-B29
.                                        (WRK-TMNFB029-NREG)
.
.
.          MOVE WRK-TIMESTAMP          TO WRK-HINIC-MNTRC
.                                        (WRK-TMNFB029-NREG)
.
.          PERFORM 1200-OBTER-DATA-HORA.
.          MOVE WRK-TIMESTAMP          TO WRK-HFNAL-MNTRC
.                                        (WRK-TMNFB029-NREG).
.
.          MOVE WRK-DESC-WORKF         TO WRK-RERRO-MNTRC
.                                        (WRK-TMNFB029-NREG)
.
.          IF WRK-SIT-FASE             NOT EQUAL 2
.             SET WRK-ERR-PROCESSO     TO TRUE
.             INITIALIZE TMNFWS03-BL-RET-BDSCO
.             MOVE 1000                TO TMNFWS03-CRETOR-PRINC-N
.             MOVE 1                   TO TMNFWS03-CRETOR-OPCAO-N
.             MOVE 'NR2-ERRO NA CRIPTOGRAFIA DA MENSAGEM'
.                                      TO TMNFWS03-RMSGEM-ERRO-X
.             PERFORM 5000-INF-BLOCO-RETORNO
.
.             MOVE 1                   TO WRK-CINDCD-RPROC-AUX
.             MOVE 'S'                 TO WRK-RPROC-CRIPTOGRAFIA
.          END-IF.
.
.     *----------------------------------------------------------------*
.      2735-99-EXIT.                   EXIT.
181218*----------------------------------------------------------------*
      *----------------------------------------------------------------*
       2800-TRATAR-ENVIO-CMARA         SECTION.
      *----------------------------------------------------------------*

           PERFORM 2810-OBTER-FILA-CAMARA.

           PERFORM 2815-ABRIR-FILA.

           IF (MQ-COMPLETION-CODE      NOT EQUAL ZEROS)
              MOVE 34                  TO WRK-FASE
              MOVE 5                   TO WRK-SIT-FASE
              INITIALIZE WRK-DESC-WORKF
              MOVE 'NAO FOI POSSIVEL ABRIR FILA DA RSFN.'
                                       TO WRK-DESC-WORKF-LN1
              MOVE 'FILA: '            TO WRK-DESC-WORKF-LN2
              MOVE WRK-MQ-FILA         TO WRK-DESC-WORKF-LN2(7:48)
              MOVE 'OPN'               TO WRK-ERR-MQ-CMD
              MOVE '0145'              TO WRK-ERR-MQ-LOCAL
              PERFORM 2811-ETAPA-ERR-FILA-CMARA
AJG           PERFORM 9700-TRATAR-ERRO-MQ
              PERFORM 3000-FINALIZAR
           END-IF.

           MOVE MQ-HANDLE-FILA         TO WRK-HANDLE-FILA.

           MOVE WRK-MQ-FILA            TO WRK-FILA-CAMARA.
           MOVE WRK-HANDLE-FILA        TO WRK-HAND-CAMARA.

           PERFORM 2820-ENVIO-CMARA.

           PERFORM 2830-MOVER-MQ-ID.

181218     IF WRK-SCRY-CONV-DOM        EQUAL '0'
              MOVE 33                  TO WRK-FASE
              MOVE 2                   TO WRK-SIT-FASE
              INITIALIZE WRK-DESC-WORKF
              MOVE 'VR2-MENSAGEM ENCAMINHADA A RSFN.'
                                       TO WRK-DESC-WORKF-LN1
              MOVE 'FILA......: '      TO WRK-DESC-WORKF-LN2
              MOVE WRK-MQ-FILA         TO WRK-DESC-WORKF-LN2(13:48)
              MOVE 'FILA REPLY: '      TO WRK-DESC-WORKF-LN3
              MOVE WRK-FILA-REPLY      TO WRK-DESC-WORKF-LN3(13:48)
              PERFORM 2840-ETAPA-ENVIO-CMARA
181218     ELSE
.             MOVE 33                  TO WRK-FASE
.             MOVE 2                   TO WRK-SIT-FASE
.             INITIALIZE WRK-DESC-WORKF
.             MOVE 'NR2-MENSAGEM ENCAMINHADA A RSFN.'
.                                      TO WRK-DESC-WORKF-LN1
.             MOVE 'FILA......: '      TO WRK-DESC-WORKF-LN2
.             MOVE WRK-MQ-FILA         TO WRK-DESC-WORKF-LN2(13:48)
.             MOVE 'FILA REPLY: '      TO WRK-DESC-WORKF-LN3
.             MOVE WRK-FILA-REPLY      TO WRK-DESC-WORKF-LN3(13:48)
.             PERFORM 2840-ETAPA-ENVIO-CMARA
181218     END-IF.

      *----------------------------------------------------------------*
       2800-99-FIM.                    EXIT.
      *----------------------------------------------------------------*
      *----------------------------------------------------------------*
       2810-OBTER-FILA-CAMARA          SECTION.
      *----------------------------------------------------------------*
           INITIALIZE TMNFB032
                      TMNFB009.

           MOVE WRK-MSG-SIST           TO CSIST-EXTER
                                       OF TMNFB032.
           MOVE WRK-MSG-EVENTO         TO CEVNTO-MSGEM
                                       OF TMNFB032.
           MOVE WRK-MSG-FLUXO          TO CFLUXO-MSGEM
                                       OF TMNFB032.

AJG12*****XMOVEXTMNFWS04-CSIST-PGTO-EMISR
AJG12      MOVE WRK-CEMISR-MSGEM       TO CISPB-PARTC-SIST
                                       OF TMNFB032.

           MOVE TMNFWE16-CDOM-MSGRA-SPB
                                       TO CDOMNO-MSGRA-SIST
                                       OF TMNFB032.

           MOVE 3                      TO CTPO-FILA-MSGRA
                                       OF TMNFB009.
           EXEC SQL
              SELECT VALUE(A.IFILA_MSGRA, ' ')
                INTO :TMNFB009.IFILA-MSGRA
                FROM DB2PRD.TFILA_COMUN_SIST A,
                     DB2PRD.TFILA_ENVIO_CMARA B
               WHERE B.CSIST_EXTER     = :TMNFB032.CSIST-EXTER
                 AND B.CEVNTO_MSGEM    = :TMNFB032.CEVNTO-MSGEM
                 AND IFNULL(B.CFLUXO_MSGEM, ' ')
                                       = :TMNFB032.CFLUXO-MSGEM
                 AND B.CDOMNO_MSGRA_SIST
                                       = :TMNFB032.CDOMNO-MSGRA-SIST
                 AND B.CISPB_PARTC_SIST
                                       = :TMNFB032.CISPB-PARTC-SIST
                 AND B.NFILA_MSGRA     = A.NFILA_MSGRA
                 AND A.CTPO_FILA_MSGRA = :TMNFB009.CTPO-FILA-MSGRA
               FETCH FIRST ROW ONLY
                WITH UR
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS AND +100)
           OR (SQLWARN0                EQUAL 'W')
               MOVE 'TFILA_ENVIO_CMARA'
                                       TO ERR-DBD-TAB
               MOVE 'SELECT'           TO ERR-FUN-COMANDO
               MOVE '0150'             TO ERR-LOCAL
               PERFORM 9200-TRATAR-ERRO-DB2
               PERFORM 3300-PROCEDIMENTOS-FINAIS
           END-IF.

           IF  SQLCODE                 NOT EQUAL ZEROS
               INITIALIZE WRK-DESC-WORKF
               MOVE 'FILA CAMARA PARA ENVIO NAO IDENTIFICADA.'
                                       TO WRK-DESC-WORKF-LN1

               STRING 'MENSAGEM: '
                       WRK-MSG
                      ' / ISPB: '
                       WRK-CEMISR-MSGEM
                      ' / DOMINIO: '
                       TMNFWE16-CDOM-MSGRA-SPB
                                       DELIMITED BY
                                       SIZE INTO WRK-DESC-WORKF-LN2
               PERFORM 2811-ETAPA-ERR-FILA-CMARA
REPROC         MOVE 3                  TO WRK-CINDCD-RPROC-AUX
REPROC         MOVE 'S'                TO WRK-RPROC-ENVIO-BACEN
               MOVE 2                  TO ERR-RETURN-CODE
               MOVE WRK-DESC-WORKF-LN1 TO ERR-TEXTO-MSG
               PERFORM 3000-FINALIZAR
           END-IF.

AJG        MOVE IFILA-MSGRA            OF TMNFB009
AJG                                    TO WRK-MQ-FILA.

      *----------------------------------------------------------------*
       2810-99-EXIT.                   EXIT.
      *----------------------------------------------------------------*
      *----------------------------------------------------------------*
       2811-ETAPA-ERR-FILA-CMARA       SECTION.
      *----------------------------------------------------------------*
AJG        MOVE WRK-TMNFB029-NREG      TO WRK-TMNFB029-NREG-AUX.
AJG        ADD 1                       TO WRK-TMNFB029-NREG-AUX.
AJG        MOVE WRK-TMNFB029-NREG-AUX  TO WRK-TMNFB029-NREG.

           SET WRK-ERR-PROCESSO        TO TRUE

           MOVE 34                     TO WRK-CFASE-PROCM
                                         (WRK-TMNFB029-NREG)

           MOVE 5                      TO WRK-SIT-FASE-B29
                                         (WRK-TMNFB029-NREG).


           MOVE ZEROS                  TO WRK-NULL-RERRO-ETAPA-MNTRC
           MOVE WRK-DESC-WORKF         TO WRK-RERRO-MNTRC
                                        (WRK-TMNFB029-NREG).

           MOVE WRK-TIMESTAMP          TO WRK-HINIC-MNTRC
                                         (WRK-TMNFB029-NREG).
           PERFORM 1200-OBTER-DATA-HORA.
           MOVE WRK-TIMESTAMP          TO WRK-HFNAL-MNTRC
                                         (WRK-TMNFB029-NREG).

           INITIALIZE TMNFWS03-BL-RET-BDSCO
           MOVE 1001                   TO TMNFWS03-CRETOR-PRINC-N.
           MOVE 1                      TO TMNFWS03-CRETOR-OPCAO-N.
           MOVE 'ISPB EMISSOR/MENSAGEM  NAO CADASTRADOS'
                                       TO TMNFWS03-RMSGEM-ERRO-X.
           PERFORM 5000-INF-BLOCO-RETORNO.

           MOVE 'N'                    TO WRK-MSG-PROBLEMA.
REPROC     MOVE 3                      TO WRK-CINDCD-RPROC-AUX
REPROC     MOVE 'S'                    TO WRK-RPROC-ENVIO-BACEN.

      *----------------------------------------------------------------*
       2811-99-EXIT.                   EXIT.
      *----------------------------------------------------------------*
      *----------------------------------------------------------------*
       2815-ABRIR-FILA                 SECTION.
      *----------------------------------------------------------------*
           MOVE '2815-ABRIR-FILA'      TO WRK-PARAGRAFO

           INITIALIZE MQ-MCA.

           MOVE 1                      TO  MQ-FORMATO.
           MOVE WRK-OPEN               TO  MQ-FUNCAO.
           MOVE WRK-PUT                TO  MQ-OPCAO.
           MOVE WRK-MQ-FILA            TO  MQ-FILA.

           MOVE ZEROS                  TO  MQ-COMPLETION-CODE
                                           MQ-REASON-CODE.

           MOVE SPACES                 TO  MQ-FILA-RESPOSTA.
           MOVE ZEROS                  TO  MQ-TEMPO-ESPERA
                                           MQ-NOTIFICACAO
                                           MQ-TIPO-MENSAGEM.

           MOVE SPACES                 TO  MQ-ID-MENSAGEM
                                           MQ-ID-DE-CORRELACAO.

           EXEC CICS
                LINK
                PROGRAM (WRK-SBAT3520)
                COMMAREA(MQ-MCA)
                LENGTH  (LENGTH OF MQ-MCA)
                NOHANDLE
           END-EXEC.

           IF EIBRESP                  NOT EQUAL DFHRESP(NORMAL)
              INITIALIZE ERRO-AREA
              MOVE '0050'              TO ERR-LOCAL
              MOVE WRK-SBAT3520        TO ERR-MODULO
              PERFORM 9300-TRATAR-ERRO-CICS
              PERFORM 3000-FINALIZAR
           END-IF.

           IF (MQ-COMPLETION-CODE      NOT EQUAL ZEROS)
              CONTINUE
           END-IF.

      *----------------------------------------------------------------*
       2815-99-FIM.                    EXIT.
      *----------------------------------------------------------------*
      *----------------------------------------------------------------*
       2820-ENVIO-CMARA                SECTION.
      *----------------------------------------------------------------*

181218     IF WRK-SCRY-CONV-DOM        EQUAL '0'
              MOVE LNK-301-LEN-AREA    TO WRK-LLBUFFER-GRAVACAO
              MOVE LNK-301-AREA(1:LNK-301-LEN-AREA)
                                       TO WRK-MQ-MSGEM-AREA
                                         (1:LNK-301-LEN-AREA)
              MOVE WRK-SBAT3520           TO WRK-MQ-MSGEM-AREA
                                         (LNK-301-LEN-AREA + 1:8)
              ADD 16                   TO WRK-LLBUFFER-GRAVACAO
181218     ELSE
.             MOVE LNK-331-LEN-AREA    TO WRK-LLBUFFER-GRAVACAO
.             MOVE LNK-331-AREA(1:LNK-331-LEN-AREA)
.                                      TO WRK-MQ-MSGEM-AREA
.                                        (1:LNK-331-LEN-AREA)
.             MOVE WRK-SBAT3520        TO WRK-MQ-MSGEM-AREA
.                                        (LNK-331-LEN-AREA + 1:8)
.             ADD 16                   TO WRK-LLBUFFER-GRAVACAO
181218     END-IF.

           SET WRK-COA-COD             TO TRUE.

           INITIALIZE WRK-FILA-REPLY
           UNSTRING WRK-MQ-FILA        DELIMITED BY '.'
                                       INTO WRK-REP-DESPR-1
                                            WRK-REP-DESPR-2
                                            WRK-REP-ISPB-CONGL
                                            WRK-REP-ISPB-CMARA
                                            WRK-REP-SEQ

           IF  WRK-REP-ISPB-CMARA       NOT EQUAL WRK-ISPB-CIP-SILOC
           AND WRK-REP-ISPB-CMARA       NOT EQUAL WRK-ISPB-CIP-ANTIGA
               MOVE WRK-REP-ISPB-CONGL  TO WRK-QM-ISPB-CONGL
               MOVE WRK-REP-SEQ         TO WRK-QM-REPLY-SEQ
               MOVE WRK-QM-SMP-REPLY    TO WRK-QM-REPLY
           ELSE
               MOVE WRK-REP-ISPB-CONGL  TO WRK-QM-CMP-CONGL
               MOVE WRK-REP-ISPB-CMARA  TO WRK-QM-CMP-DEST
               MOVE WRK-REP-SEQ         TO WRK-QM-CMP-SEQ
               MOVE WRK-QM-CMP-REPLY    TO WRK-QM-REPLY
           END-IF.

           PERFORM 2825-TRATAR-PUT.

           IF MQ-COMPLETION-CODE       NOT EQUAL ZEROS
181218        IF WRK-SCRY-CONV-DOM        EQUAL '0'
                 MOVE 34                  TO WRK-FASE
                 MOVE 5                   TO WRK-SIT-FASE

                 INITIALIZE WRK-DESC-WORKF
                 MOVE 'VR2-ERRO AO ENCAMINHAR MENSAGEM A RSFN.'
                                       TO WRK-DESC-WORKF-LN1
                 MOVE 'FILA: '            TO WRK-DESC-WORKF-LN2
                 MOVE WRK-MQ-FILA         TO WRK-DESC-WORKF-LN2(7:48)
                 PERFORM 2811-ETAPA-ERR-FILA-CMARA
                 MOVE 'PUT'               TO WRK-ERR-MQ-CMD
                 MOVE '0160'              TO WRK-ERR-MQ-LOCAL
                 PERFORM 9700-TRATAR-ERRO-MQ
                 PERFORM 3000-FINALIZAR
181218        ELSE
.                MOVE 34                  TO WRK-FASE
.                MOVE 5                   TO WRK-SIT-FASE
.
.                INITIALIZE WRK-DESC-WORKF
.                MOVE 'NR2-ERRO AO ENCAMINHAR MENSAGEM A RSFN.'
.                                      TO WRK-DESC-WORKF-LN1
.                MOVE 'FILA: '            TO WRK-DESC-WORKF-LN2
.                MOVE WRK-MQ-FILA         TO WRK-DESC-WORKF-LN2(7:48)
.                PERFORM 2811-ETAPA-ERR-FILA-CMARA
.                MOVE 'PUT'               TO WRK-ERR-MQ-CMD
.                MOVE '2820'              TO WRK-ERR-MQ-LOCAL
.                PERFORM 9700-TRATAR-ERRO-MQ
.                PERFORM 3000-FINALIZAR
.             END-IF
181218     END-IF.

      *----------------------------------------------------------------*
       2820-99-EXIT.                   EXIT.
      *----------------------------------------------------------------*
      *----------------------------------------------------------------*
       2825-TRATAR-PUT                 SECTION.
      *----------------------------------------------------------------*
           MOVE '2825-TRATAR-PUT'      TO WRK-PARAGRAFO

           MOVE WRK-PUT                TO MQ-FUNCAO.
           MOVE SPACES                 TO MQ-OPCAO.
           MOVE WRK-HANDLE-FILA        TO MQ-HANDLE-FILA.
           MOVE WRK-MQ-FILA            TO MQ-FILA.
           MOVE WRK-NOTIFICACAO        TO MQ-NOTIFICACAO.
           MOVE WRK-FILA-REPLY         TO MQ-FILA-RESPOSTA.
           MOVE WRK-QM-REPLY           TO MQ-QM-RESPOSTA.

           MOVE 1                      TO MQ-FORMATO.
           MOVE 8                      TO MQ-TIPO-MENSAGEM.
           MOVE ZEROS                  TO MQ-PRIORIDADE-MSG.
           MOVE 'N'                    TO MQ-FORCA-IDS.

           MOVE ZEROS                  TO MQ-OPCOES-DE-PUT
                                          MQ-COMPLETION-CODE
                                          MQ-REASON-CODE
                                          MQ-TEMPO-ESPERA.

           MOVE SPACES                 TO MQ-ID-MENSAGEM
                                          MQ-ID-DE-CORRELACAO
                                          MQ-FORMATO-MSG.

           SET WRK-PTR-GRAVACAO        TO ADDRESS
                                       OF WRK-MQ-MSGEM
           MOVE WRK-LLBUFFER-GRAVACAO  TO MQ-TAMANHO-BUFFER.
           MOVE WRK-PTR-GRAVACAO-COMP  TO MQ-PTR-BUFFER-COMP.
           MOVE ZEROS                  TO MQ-TAMANHO-BUFFER-LIDO.

           EXEC CICS
                LINK
                PROGRAM (WRK-SBAT3520)
                COMMAREA(MQ-MCA)
                LENGTH  (LENGTH OF MQ-MCA)
                NOHANDLE
           END-EXEC.

      *
           IF EIBRESP                  NOT EQUAL DFHRESP(NORMAL)
REPROC        MOVE 3                   TO WRK-CINDCD-RPROC-AUX
REPROC        MOVE 'S'                 TO WRK-RPROC-ENVIO-BACEN
              INITIALIZE ERRO-AREA
              MOVE WRK-SBAT3520        TO ERR-MODULO
              MOVE '0055'              TO ERR-LOCAL
              PERFORM 9300-TRATAR-ERRO-CICS
              PERFORM 3000-FINALIZAR
           END-IF.

      *----------------------------------------------------------------*
       2825-99-FIM.                    EXIT.
      *----------------------------------------------------------------*
      *----------------------------------------------------------------*
       2826-TRATAR-PUT-UNICO           SECTION.
      *----------------------------------------------------------------*
           MOVE '2826-TRATAR-PUT-UNICO'
                                       TO WRK-PARAGRAFO

           MOVE WRK-PUT                TO MQ-FUNCAO.
           MOVE 'UNICO'                TO MQ-OPCAO.
           MOVE WRK-HANDLE-FILA        TO MQ-HANDLE-FILA.
           MOVE WRK-MQ-FILA            TO MQ-FILA.
           MOVE WRK-NOTIFICACAO        TO MQ-NOTIFICACAO.
           MOVE WRK-FILA-REPLY         TO MQ-FILA-RESPOSTA.
           MOVE WRK-QM-REPLY           TO MQ-QM-RESPOSTA.

           MOVE 1                      TO MQ-FORMATO.
           MOVE 8                      TO MQ-TIPO-MENSAGEM.
           MOVE ZEROS                  TO MQ-PRIORIDADE-MSG.
           MOVE 'N'                    TO MQ-FORCA-IDS.
           MOVE 2                      TO MQ-OPCOES-DE-PUT
           MOVE ZEROS                  TO MQ-COMPLETION-CODE
                                          MQ-REASON-CODE
                                          MQ-TEMPO-ESPERA.

           MOVE SPACES                 TO MQ-ID-MENSAGEM
                                          MQ-ID-DE-CORRELACAO
                                          MQ-FORMATO-MSG.

           MOVE WRK-LLBUFFER-GRAVACAO  TO MQ-TAMANHO-BUFFER.
           MOVE WRK-PTR-GRAVACAO-COMP  TO MQ-PTR-BUFFER-COMP.
           MOVE ZEROS                  TO MQ-TAMANHO-BUFFER-LIDO.

           EXEC CICS
                LINK
                PROGRAM (WRK-SBAT3520)
                COMMAREA(MQ-MCA)
                LENGTH  (LENGTH OF MQ-MCA)
                NOHANDLE
           END-EXEC.

      *
           IF EIBRESP                  NOT EQUAL DFHRESP(NORMAL)
REPROC        MOVE 3                   TO WRK-CINDCD-RPROC-AUX
REPROC        MOVE 'S'                 TO WRK-RPROC-ENVIO-BACEN
              INITIALIZE ERRO-AREA
              MOVE WRK-SBAT3520        TO ERR-MODULO
              MOVE '0055'              TO ERR-LOCAL
              PERFORM 9300-TRATAR-ERRO-CICS
              PERFORM 3000-FINALIZAR
           END-IF.

      *----------------------------------------------------------------*
       2826-99-FIM.                    EXIT.
      *----------------------------------------------------------------*
      *----------------------------------------------------------------*
       2830-MOVER-MQ-ID                SECTION.
      *----------------------------------------------------------------*

           IF MQ-ID-MENSAGEM           EQUAL SPACES
              GO                       TO 2830-99-FIM
           END-IF

           INITIALIZE TMNFB053.

           PERFORM 2831-CONVERTER-MQ-ID.

           MOVE 2                      TO CIDTFD-TPO-MSGEM
                                       OF TMNFB053.

           MOVE WRK-NUM-UNICO-TMNF     TO CMNTRC-MSGRA
                                       OF TMNFB053

           MOVE -1                     TO WRK-NULL-HRECBD-MSGEM
                                          WRK-NULL-HCHEGD-MSGEM.

      *----------------------------------------------------------------*
       2830-99-FIM.                    EXIT.
      *----------------------------------------------------------------*
      *----------------------------------------------------------------*
       2831-CONVERTER-MQ-ID            SECTION.
      *----------------------------------------------------------------*
           MOVE '2831-CONVERTER-MQ-ID' TO WRK-PARAGRAFO

           INITIALIZE WRK-AREA-TMNF5290
      *
           MOVE 48                     TO TMNFWE04-ACAO
           MOVE MQ-ID-MENSAGEM         TO TMNFWE04-MQID-24
                                          CMNTRC-TRFGO-ESTRT
                                       OF TMNFB053
           MOVE SPACES                 TO TMNFWE04-MQID-48

           EXEC CICS LINK
               PROGRAM (WRK-TMNF5290)
               COMMAREA(WRK-AREA-TMNF5290)
               LENGTH  (LENGTH OF WRK-AREA-TMNF5290)
               NOHANDLE
           END-EXEC.

           IF EIBRESP                  NOT EQUAL DFHRESP(NORMAL)
              MOVE WRK-TMNF5290        TO ERR-MODULO
              MOVE '0175'              TO ERR-LOCAL
              PERFORM 9300-TRATAR-ERRO-CICS
              PERFORM 3300-PROCEDIMENTOS-FINAIS
           END-IF.
      *
      *----------------------------------------------------------------*
       2831-99-FIM.                    EXIT.
      *----------------------------------------------------------------*
      *----------------------------------------------------------------*
       2840-ETAPA-ENVIO-CMARA          SECTION.
      *----------------------------------------------------------------*
AJG        MOVE WRK-TMNFB029-NREG      TO WRK-TMNFB029-NREG-AUX.
AJG        ADD 1                       TO WRK-TMNFB029-NREG-AUX.
AJG        MOVE WRK-TMNFB029-NREG-AUX  TO WRK-TMNFB029-NREG.

           MOVE WRK-FASE               TO WRK-CFASE-PROCM
                                         (WRK-TMNFB029-NREG).

           MOVE WRK-SIT-FASE           TO WRK-SIT-FASE-B29
                                         (WRK-TMNFB029-NREG).

           MOVE WRK-TIMESTAMP          TO WRK-HINIC-MNTRC
                                         (WRK-TMNFB029-NREG).

           PERFORM 1200-OBTER-DATA-HORA.
           MOVE WRK-TIMESTAMP          TO WRK-HFNAL-MNTRC
                                         (WRK-TMNFB029-NREG).

           MOVE ZEROS                  TO WRK-NULL-RERRO-ETAPA-MNTRC
           MOVE WRK-DESC-WORKF         TO WRK-RERRO-MNTRC
                                         (WRK-TMNFB029-NREG)

           INITIALIZE TMNFWS03-BL-RET-BDSCO

           IF WRK-SIT-FASE             NOT EQUAL 2
              SET WRK-ERR-PROCESSO     TO TRUE
              MOVE 2                   TO ERR-RETURN-CODE

              INITIALIZE TMNFWS03-BL-RET-BDSCO
              MOVE 1000                TO TMNFWS03-CRETOR-PRINC-N
              MOVE 1                   TO TMNFWS03-CRETOR-OPCAO-N
              MOVE WRK-DESC-WORKF      TO TMNFWS03-RMSGEM-ERRO-X
              MOVE 3                   TO WRK-CINDCD-RPROC-AUX
              MOVE 'N'                 TO WRK-MSG-PROBLEMA
REPROC        MOVE 'S'                 TO WRK-RPROC-ENVIO-BACEN
           END-IF.

           PERFORM 5000-INF-BLOCO-RETORNO.

      *----------------------------------------------------------------*
       2840-99-EXIT.                   EXIT.
      *----------------------------------------------------------------*
      *----------------------------------------------------------------*
       2900-ENVIAR-COPIA-OPEN          SECTION.
      *----------------------------------------------------------------*
           IF TMNFWS05-MSG(1:3)        EQUAL 'DDA'
              GO                       TO 2900-99-FIM
           END-IF.

           PERFORM 2910-OBTER-FILA-OPEN.

           IF RVLR-DISCT-CARAC         OF TMNFB019
                                       NOT EQUAL SPACES
181218        IF WRK-SCRY-CONV-DOM        EQUAL '0'
                 PERFORM 2920-ENVIAR-MSG-OPEN
                 INITIALIZE WRK-DESC-WORKF
                 MOVE 'VR2-COPIA ENVIADA PELA FILA '
                                       TO WRK-DESC-WORKF-LN1
                 MOVE WRK-MQ-FILA      TO WRK-DESC-WORKF-LN2

                 MOVE 49               TO WRK-FASE
                 MOVE 2                TO WRK-SIT-FASE
                 PERFORM 2930-ETAPA-ENVIO-OPEN
181218        ELSE
.                PERFORM 2920-ENVIAR-MSG-OPEN
.                INITIALIZE WRK-DESC-WORKF
.                MOVE 'NR2-COPIA ENVIADA PELA FILA '
.                                      TO WRK-DESC-WORKF-LN1
.                MOVE WRK-MQ-FILA      TO WRK-DESC-WORKF-LN2
.
.                MOVE 49               TO WRK-FASE
.                MOVE 2                TO WRK-SIT-FASE
.                PERFORM 2930-ETAPA-ENVIO-OPEN
181218        END-IF
           END-IF.

      *----------------------------------------------------------------*
       2900-99-FIM.                    EXIT.
      *----------------------------------------------------------------*
      *----------------------------------------------------------------*
       2910-OBTER-FILA-OPEN            SECTION.
      *----------------------------------------------------------------*

           INITIALIZE TMNFB019.

           MOVE 'FILA_OP_E_MSGEM'      TO CVLR-DISCT-CARAC
                                       OF TMNFB019.

           EXEC SQL
              SELECT VALUE(RVLR_DISCT_CARAC, ' ')
                INTO :TMNFB019.RVLR-DISCT-CARAC
                FROM DB2PRD.TVLR_DISCT_CARAC
               WHERE CVLR_DISCT_CARAC  = :TMNFB019.CVLR-DISCT-CARAC
               FETCH FIRST ROW ONLY
                WITH UR
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS AND +100)
           OR (SQLWARN0                EQUAL 'W')
               INITIALIZE ERRO-AREA
               MOVE 'TMNTRC_MSGRA'     TO ERR-DBD-TAB
               MOVE 'SELECT'           TO ERR-FUN-COMANDO
               MOVE '0180'             TO ERR-LOCAL
               PERFORM 9200-TRATAR-ERRO-DB2
               PERFORM 3300-PROCEDIMENTOS-FINAIS
           END-IF.

           IF SQLCODE                  EQUAL +100
              MOVE SPACES              TO RVLR-DISCT-CARAC
                                       OF TMNFB019
           END-IF.

      *----------------------------------------------------------------*
       2910-99-FIM.                    EXIT.
      *----------------------------------------------------------------*
      *----------------------------------------------------------------*
       2920-ENVIAR-MSG-OPEN            SECTION.
      *----------------------------------------------------------------*

           PERFORM 2921-FORMATAR-AREA-OPEN.

           MOVE RVLR-DISCT-CARAC       OF TMNFB019
                                       TO WRK-MQ-FILA.

           MOVE WRK-MQ-FILA            TO WRK-FILA-OPEN.
           MOVE WRK-HANDLE-FILA        TO WRK-HAND-OPEN.

           PERFORM 2922-ENVIAR-FILA-OPEN.

      *----------------------------------------------------------------*
       2920-99-FIM.                    EXIT.
      *----------------------------------------------------------------*
      *----------------------------------------------------------------*
       2921-FORMATAR-AREA-OPEN         SECTION.
      *----------------------------------------------------------------*
           INITIALIZE TMNF01-REGISTRO.

           MOVE 9                      TO TMNF01-LAYOUT.
           MOVE WRK-NUM-UNICO-TMNF     TO TMNF01-CMNTRC-MSGRA.
           MOVE ZEROS                  TO TMNF01-NMNTRC-TRFGO-MSGEM.
           MOVE TMNFW820-S-NESTRT-INFO TO TMNF01-NESTRT-INFO-BACEN.
           MOVE TMNFW820-S-NVRSAO-ESTRT
                                       TO TMNF01-NVRSAO-ESTRT-INFO.

           IF  TMNFWE16-ENTRADA-MANUAL EQUAL 'S'
               MOVE WRK-ENVIO-MANUAL   TO TMNF01-CINDCD-ORIGE-TRFGO
           ELSE
               MOVE WRK-ENVIO-LEGADO   TO TMNF01-CINDCD-ORIGE-TRFGO
           END-IF.

           MOVE WRK-CSNLZD-MSGRA       TO TMNF01-CSNLZD-MSGRA.
           MOVE SPACES                 TO TMNF01-HCONF-ENVIO-MSGEM.
           MOVE SPACES                 TO TMNF01-HCONF-LEITU-MSGEM.
           MOVE WRK-TIMESTAMP          TO TMNF01-HPROCM-MSGEM
                                          TMNF01-HULT-ATULZ.
           MOVE TMNFWE16-HOCORR-WKFLW  TO TMNF01-HMOVTO-INFO-MSGEM.
           MOVE SPACES                 TO TMNF01-HPROCM-MSGEM(11:1)
                                          TMNF01-HULT-ATULZ(11:1)
                                          TMNF01-HMOVTO-INFO-MSGEM
                                          (11:1)
           MOVE ':'                    TO TMNF01-HPROCM-MSGEM(14:1)
                                          TMNF01-HULT-ATULZ(14:1)
                                          TMNF01-HMOVTO-INFO-MSGEM
                                          (14:1)
           MOVE ':'                    TO TMNF01-HPROCM-MSGEM(17:1)
                                          TMNF01-HULT-ATULZ(17:1)
                                          TMNF01-HMOVTO-INFO-MSGEM
                                          (17:1)
           MOVE WRK-CDSTNA-MSGEM       TO TMNF01-CISPB-DSTNA-MSGEM.
           MOVE WRK-CEMISR-MSGEM       TO TMNF01-CISPB-EMISR-MSGEM.
           MOVE WRK-CTRLIF             TO TMNF01-CCTRL-RQUIS-INSTC.
           MOVE WRK-NUOP               TO TMNF01-CUNIC-TRFGO-MSGEM.
           MOVE TMNFWE04-MQID-48       TO TMNF01-RIDTFD-MSGEM.
           MOVE WRK-MSG                TO TMNF01-RMNTRC-TRFGO-MSGEM.
           MOVE ZEROS
                                       TO TMNF01-CIDTFD-PROT-ARQ.
           MOVE ZEROS                  TO TMNF01-CINDCD-FORMT-MSGEM.
           MOVE TMNFWE16-CDOM-MSGRA-SPB
                                       TO TMNF01-CDOMNO-MSGRA-SIST.
           MOVE TMNFW820-S-XML         TO TMNF01-BCONTD-MSGEM-XML.
           MOVE TMNFWE05-MENSAGEM      TO TMNF01-BCONTD-MSGEM-TXT.

      *----------------------------------------------------------------*
       2921-99-FIM.                    EXIT.
      *----------------------------------------------------------------*
      *----------------------------------------------------------------*
       2922-ENVIAR-FILA-OPEN           SECTION.
      *----------------------------------------------------------------*

           MOVE LENGTH                 OF TMNF01-BLOCO-CTRL
                                       TO WRK-LLBUFFER-GRAVACAO

181218     IF WRK-SCRY-CONV-DOM        EQUAL '0'
              MOVE TMNF01-BLOCO-CTRL   TO WRK-MQ-MSGEM-AREA
                                         (1:LNK-301-LEN-AREA)
181218     ELSE
181218        MOVE TMNF01-BLOCO-CTRL   TO WRK-MQ-MSGEM-AREA
181218                                   (1:LNK-331-LEN-AREA)
181218     END-IF.

           MOVE WRK-SBAT3520           TO WRK-MQ-MSGEM-AREA
                                         (WRK-LLBUFFER-GRAVACAO + 1:8)
           ADD 16                      TO WRK-LLBUFFER-GRAVACAO

           SET WRK-SEM-NOTIF           TO TRUE.
           MOVE SPACES                 TO WRK-FILA-REPLY
                                          WRK-QM-REPLY

           PERFORM 2826-TRATAR-PUT-UNICO.

           IF MQ-COMPLETION-CODE       NOT EQUAL ZEROS
181218        IF WRK-SCRY-CONV-DOM     EQUAL '0'
                 MOVE 2                TO ERR-RETURN-CODE
                 MOVE 50               TO WRK-FASE
FLAVIO           MOVE 'S'              TO WRK-ERRO-COPIA
                 MOVE 5                TO WRK-SIT-FASE
                 INITIALIZE WRK-DESC-WORKF
                 MOVE 'VR2-ERRO AO ENCAMINHAR COPIA PARA OPEN.'
                                       TO WRK-DESC-WORKF-LN1
                 MOVE 'FILA: '         TO WRK-DESC-WORKF-LN2
                 MOVE WRK-MQ-FILA      TO WRK-DESC-WORKF-LN2(7:48)
                 PERFORM 2930-ETAPA-ENVIO-OPEN
                 MOVE 'PUT'            TO WRK-ERR-MQ-CMD
                 MOVE '0185'           TO WRK-ERR-MQ-LOCAL
                 PERFORM 9700-TRATAR-ERRO-MQ
                 PERFORM 3000-FINALIZAR
181218        ELSE
.                MOVE 2                TO ERR-RETURN-CODE
.                MOVE 50               TO WRK-FASE
.                MOVE 'S'              TO WRK-ERRO-COPIA
.                MOVE 5                TO WRK-SIT-FASE
.                INITIALIZE WRK-DESC-WORKF
.                MOVE 'NR2-ERRO AO ENCAMINHAR COPIA PARA OPEN.'
.                                      TO WRK-DESC-WORKF-LN1
.                MOVE 'FILA: '         TO WRK-DESC-WORKF-LN2
.                MOVE WRK-MQ-FILA      TO WRK-DESC-WORKF-LN2(7:48)
.                PERFORM 2930-ETAPA-ENVIO-OPEN
.                MOVE 'PUT'            TO WRK-ERR-MQ-CMD
.                MOVE '2922'           TO WRK-ERR-MQ-LOCAL
.                PERFORM 9700-TRATAR-ERRO-MQ
.                PERFORM 3000-FINALIZAR
181218        END-IF
           END-IF.

      *----------------------------------------------------------------*
       2922-99-FIM.                    EXIT.
      *----------------------------------------------------------------*
      *----------------------------------------------------------------*
       2930-ETAPA-ENVIO-OPEN           SECTION.
      *----------------------------------------------------------------*

AJG        MOVE WRK-TMNFB029-NREG      TO WRK-TMNFB029-NREG-AUX.
AJG        ADD 1                       TO WRK-TMNFB029-NREG-AUX.
AJG        MOVE WRK-TMNFB029-NREG-AUX  TO WRK-TMNFB029-NREG.

           MOVE WRK-FASE               TO WRK-CFASE-PROCM
                                         (WRK-TMNFB029-NREG)

           MOVE WRK-SIT-FASE           TO WRK-SIT-FASE-B29
                                         (WRK-TMNFB029-NREG)

           MOVE WRK-DESC-WORKF         TO WRK-RERRO-MNTRC
                                         (WRK-TMNFB029-NREG)

           MOVE WRK-TIMESTAMP          TO WRK-HINIC-MNTRC
                                         (WRK-TMNFB029-NREG)

           PERFORM 1200-OBTER-DATA-HORA.
           MOVE WRK-TIMESTAMP          TO WRK-HFNAL-MNTRC
                                         (WRK-TMNFB029-NREG).

           IF WRK-SIT-FASE             NOT EQUAL 2
              SET WRK-ERR-PROCESSO     TO TRUE
              MOVE 2                   TO ERR-RETURN-CODE

              INITIALIZE TMNFWS03-BL-RET-BDSCO
              MOVE 1000                TO TMNFWS03-CRETOR-PRINC-N
              MOVE 1                   TO TMNFWS03-CRETOR-OPCAO-N
              MOVE WRK-DESC-WORKF      TO TMNFWS03-RMSGEM-ERRO-X
FLAVIO        IF WRK-ERRO-COPIA        EQUAL 'S'
FLAVIO           MOVE ZEROS            TO WRK-CINDCD-RPROC-AUX
FLAVIO           MOVE 'N'              TO WRK-RPROC-ENVIO-BACEN
FLAVIO        ELSE
                 MOVE 3                TO WRK-CINDCD-RPROC-AUX
REPROC           MOVE 'S'              TO WRK-RPROC-ENVIO-BACEN
FLAVIO        END-IF

              MOVE 'N'                 TO WRK-MSG-PROBLEMA
              PERFORM 5000-INF-BLOCO-RETORNO
           END-IF.

      *----------------------------------------------------------------*
       2930-99-FIM.                    EXIT.
      *----------------------------------------------------------------*
      *----------------------------------------------------------------*
       2950-FECHAR-FILA                SECTION.
      *----------------------------------------------------------------*
           MOVE '2950-FECHAR-FILA'     TO WRK-PARAGRAFO

           INITIALIZE MQ-MCA.

           MOVE WRK-CLOSE              TO  MQ-FUNCAO.
           MOVE SPACES                 TO  MQ-OPCAO.
           MOVE WRK-HANDLE-FILA        TO  MQ-HANDLE-FILA.
           MOVE WRK-MQ-FILA            TO  MQ-FILA.

           EXEC CICS
                LINK
                PROGRAM (WRK-SBAT3520)
                COMMAREA(MQ-MCA)
                LENGTH  (LENGTH OF MQ-MCA)
           END-EXEC.


           IF EIBRESP                  NOT EQUAL DFHRESP(NORMAL)
              INITIALIZE ERRO-AREA
              MOVE '0070'              TO ERR-LOCAL
              MOVE WRK-SBAT3520        TO ERR-MODULO
              PERFORM 9300-TRATAR-ERRO-CICS
           END-IF.

      *----------------------------------------------------------------*
       2950-99-FIM.                    EXIT.
      *----------------------------------------------------------------*
      *----------------------------------------------------------------*
       3000-FINALIZAR                  SECTION.
      *----------------------------------------------------------------*

           IF  WRK-NUOP                NOT EQUAL LOW-VALUES
           AND ERR-RETURN-CODE         NOT EQUAL 1
               PERFORM 3100-GRAVAR-MONITORACAO

               PERFORM 3200-GRAVAR-ETAPAS
           END-IF.

           IF  WRK-MSG-PROBLEMA        EQUAL 'S'
           AND ERR-RETURN-CODE         NOT EQUAL 1
              PERFORM 4100-GRAVAR-INCONSISTENTE
           END-IF.

REPROC     IF  WRK-RPROC-CONVERSAO
.                                      EQUAL 'S'
.              PERFORM 6000-MOVER-RPROC-TXT
.          END-IF.
.
.          IF  WRK-RPROC-CRIPTOGRAFIA
.                                      EQUAL 'S'
.              PERFORM 6100-MOVER-RPROC-XML
.          END-IF.
.
.          IF  WRK-RPROC-ENVIO-BACEN
.                                      EQUAL 'S'
.              PERFORM 6200-MOVER-RPROC-CRIPTO
REPROC     END-IF.

           PERFORM 3300-PROCEDIMENTOS-FINAIS.

      *----------------------------------------------------------------*
       3000-99-FIM.                    EXIT.
      *----------------------------------------------------------------*
      *----------------------------------------------------------------*
       3100-GRAVAR-MONITORACAO         SECTION.
      *----------------------------------------------------------------*

           PERFORM 3110-MOVE-MSG-TXT.

           IF  TMNFW820-S-TAM-XML      GREATER ZEROS
           AND WRK-RPROC-CONVERSAO
                                       NOT EQUAL 'S'
              PERFORM 3140-MOVER-MSG-XML
           END-IF.

           PERFORM 3400-INCLUIR-MNTRC-NOVA

           IF  CMNTRC-MSGRA           OF TMNFB053
                                      NOT EQUAL LOW-VALUES
           AND CMNTRC-MSGRA           OF TMNFB053
                                      NOT EQUAL SPACES
           AND CMNTRC-TRFGO-ESTRT     OF TMNFB053
                                      NOT EQUAL SPACES
               PERFORM 3150-GRAVAR-MQ-ID
           END-IF.

           IF WRK-CTRLIF(5:4)          NOT EQUAL 'CARG'
              PERFORM 3160-EXECUTAR-TRANS-SPBN
           END-IF.

      *----------------------------------------------------------------*
       3100-99-EXIT.                   EXIT.
      *----------------------------------------------------------------*
      *----------------------------------------------------------------*
       3110-MOVE-MSG-TXT               SECTION.
      *----------------------------------------------------------------*
           COMPUTE WRK-TAMANHO-REAL = TMNFWE16-TAM-TOTAL
                                    - LENGTH OF TMNFWE16-REGISTRO
                                    - LENGTH OF TMNFWE09-ERRO-AREA
                                             OF DFHCOMMAREA

           MOVE WRK-TAMANHO-REAL       TO WRK-MSG-TXT-LENGTH.
           MOVE TMNFWE05-MENSAGEM(1:WRK-TAMANHO-REAL)
                                       TO WRK-MSG-TXT-DATA.

      *----------------------------------------------------------------*
       3110-99-EXIT.                   EXIT.
      *----------------------------------------------------------------*
      *----------------------------------------------------------------*
       3140-MOVER-MSG-XML              SECTION.
      *----------------------------------------------------------------*
           MOVE TMNFW820-S-TAM-XML     TO WRK-TAMANHO-REAL.
           MOVE WRK-TAMANHO-REAL       TO WRK-MSG-XML-LENGTH.
           MOVE TMNFW820-S-XML(1:WRK-TAMANHO-REAL)
                                       TO WRK-MSG-XML-DATA.

      *----------------------------------------------------------------*
       3140-99-EXIT.                   EXIT.
      *----------------------------------------------------------------*
      *----------------------------------------------------------------*
       3150-GRAVAR-MQ-ID              SECTION.
      *----------------------------------------------------------------*
           EXEC SQL
               SET :WRK-NETAPA-ESTRT =
              NEXT VALUE FOR DB2PRD.SEQ_TMNFN000_TETAPA_MNTRC_ESTRT
           END-EXEC.

           EVALUATE TRUE
              WHEN WRK-DOMINGO
                 PERFORM 3152-ETAPA-ESTRT-DOMINGO

              WHEN WRK-SEGUNDA
                 PERFORM 3153-ETAPA-ESTRT-SEGUNDA

              WHEN WRK-TERCA
                 PERFORM 3154-ETAPA-ESTRT-TERCA

              WHEN WRK-QUARTA
                 PERFORM 3155-ETAPA-ESTRT-QUARTA

              WHEN WRK-QUINTA
                 PERFORM 3156-ETAPA-ESTRT-QUINTA

              WHEN WRK-SEXTA
                 PERFORM 3157-ETAPA-ESTRT-SEXTA

              WHEN WRK-SABADO
                 PERFORM 3158-ETAPA-ESTRT-SABADO

              WHEN OTHER
                 PERFORM 3151-ETAPA-ESTRT-CONTG
           END-EVALUATE.

           IF SQLCODE                  NOT EQUAL ZEROS
              MOVE WRK-PGM-CHAMADOR    TO ERR-PGM
              MOVE WRK-PROGRAMA        TO ERR-MODULO
              MOVE SQLCODE             TO ERR-SQL-CODE
              MOVE 'DB2'               TO ERR-TIPO-ACESSO
              MOVE SPACES              TO ERR-SEGM
              MOVE SQLCA               TO ERR-SQLCA
              MOVE WRK-PROGRAMA        TO ERR-MODULO
              MOVE 'INSERT'            TO ERR-FUN-COMANDO
              PERFORM 9500-GERAR-LOG-ERRO

              PERFORM 3151-ETAPA-ESTRT-CONTG
           END-IF.

      *----------------------------------------------------------------*
       3150-99-EXIT.                   EXIT.
      *----------------------------------------------------------------*
      *----------------------------------------------------------------*
       3151-ETAPA-ESTRT-CONTG          SECTION.
      *----------------------------------------------------------------*

           EXEC SQL
               INSERT INTO DB2PRD.TETAPA_ESTRT_CONTG
                   ( NETAPA_MNTRC_ESTRT
                   , CPTCAO_TBELA
                   , CMNTRC_TRFGO_ESTRT
                   , CMNTRC_MSGRA
                   , CIDTFD_TPO_MSGEM
                   , HENVIO_MSGEM
                   , HRECBD_MSGEM
                   , HCHEGD_MSGEM)
               VALUES
                  (  :WRK-NETAPA-ESTRT
                   , :WRK-CPTCAO
                   , :TMNFB053.CMNTRC-TRFGO-ESTRT
                   , :TMNFB053.CMNTRC-MSGRA
                   , :TMNFB053.CIDTFD-TPO-MSGEM
                   , CURRENT TIMESTAMP
                   , :TMNFB053.HRECBD-MSGEM
                        :WRK-NULL-HRECBD-MSGEM
                   , :TMNFB053.HCHEGD-MSGEM
                        :WRK-NULL-HCHEGD-MSGEM)
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS)
           OR (SQLWARN0                EQUAL 'W')
               MOVE 'TETAPA_ESTRT_CONTG'
                                       TO ERR-DBD-TAB
               MOVE 'INSERT'           TO ERR-FUN-COMANDO
               MOVE '3151'             TO ERR-LOCAL
               PERFORM 9200-TRATAR-ERRO-DB2
               PERFORM 3300-PROCEDIMENTOS-FINAIS
           END-IF.

      *----------------------------------------------------------------*
       3151-99-FIM.                    EXIT.
      *----------------------------------------------------------------*
      *----------------------------------------------------------------*
       3152-ETAPA-ESTRT-DOMINGO        SECTION.
      *----------------------------------------------------------------*

           EXEC SQL
               INSERT INTO DB2PRD.TETAPA_ESTRT_DMNGO
                   (NETAPA_MNTRC_ESTRT
                   ,CPTCAO_TBELA
                   ,CMNTRC_TRFGO_ESTRT
                   ,CMNTRC_MSGRA
                   ,CIDTFD_TPO_MSGEM
                   ,HENVIO_MSGEM
                   ,HRECBD_MSGEM
                   ,HCHEGD_MSGEM)
               VALUES
                  ( :WRK-NETAPA-ESTRT
                   ,:WRK-CPTCAO
                   ,:TMNFB053.CMNTRC-TRFGO-ESTRT
                   ,:TMNFB053.CMNTRC-MSGRA
                   ,:TMNFB053.CIDTFD-TPO-MSGEM
                   ,CURRENT TIMESTAMP
                   ,:TMNFB053.HRECBD-MSGEM
                        :WRK-NULL-HRECBD-MSGEM
                   ,:TMNFB053.HCHEGD-MSGEM
                        :WRK-NULL-HCHEGD-MSGEM)
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS)
           OR (SQLWARN0                EQUAL 'W')
               MOVE 'TETAPA_ESTRT_DMNGO'
                                       TO ERR-DBD-TAB
               MOVE '3152'             TO ERR-LOCAL
           END-IF.

      *----------------------------------------------------------------*
       3152-99-FIM.                    EXIT.
      *----------------------------------------------------------------*
      *----------------------------------------------------------------*
       3153-ETAPA-ESTRT-SEGUNDA        SECTION.
      *----------------------------------------------------------------*

           EXEC SQL
               INSERT INTO DB2PRD.TETAPA_ESTRT_SEGDA
                   (NETAPA_MNTRC_ESTRT
                   ,CPTCAO_TBELA
                   ,CMNTRC_TRFGO_ESTRT
                   ,CMNTRC_MSGRA
                   ,CIDTFD_TPO_MSGEM
                   ,HENVIO_MSGEM
                   ,HRECBD_MSGEM
                   ,HCHEGD_MSGEM)
               VALUES
                  ( :WRK-NETAPA-ESTRT
                   ,:WRK-CPTCAO
                   ,:TMNFB053.CMNTRC-TRFGO-ESTRT
                   ,:TMNFB053.CMNTRC-MSGRA
                   ,:TMNFB053.CIDTFD-TPO-MSGEM
                   ,CURRENT TIMESTAMP
                   ,:TMNFB053.HRECBD-MSGEM
                        :WRK-NULL-HRECBD-MSGEM
                   ,:TMNFB053.HCHEGD-MSGEM
                        :WRK-NULL-HCHEGD-MSGEM)
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS)
           OR (SQLWARN0                EQUAL 'W')
               MOVE 'TETAPA_ESTRT_SEGDA'
                                       TO ERR-DBD-TAB
               MOVE '3153'             TO ERR-LOCAL
           END-IF.

      *----------------------------------------------------------------*
       3153-99-FIM.                    EXIT.
      *----------------------------------------------------------------*
      *----------------------------------------------------------------*
       3154-ETAPA-ESTRT-TERCA          SECTION.
      *----------------------------------------------------------------*

           EXEC SQL
               INSERT INTO DB2PRD.TETAPA_ESTRT_TRC
                   (NETAPA_MNTRC_ESTRT
                   ,CPTCAO_TBELA
                   ,CMNTRC_TRFGO_ESTRT
                   ,CMNTRC_MSGRA
                   ,CIDTFD_TPO_MSGEM
                   ,HENVIO_MSGEM
                   ,HRECBD_MSGEM
                   ,HCHEGD_MSGEM)
               VALUES
                  ( :WRK-NETAPA-ESTRT
                   ,:WRK-CPTCAO
                   ,:TMNFB053.CMNTRC-TRFGO-ESTRT
                   ,:TMNFB053.CMNTRC-MSGRA
                   ,:TMNFB053.CIDTFD-TPO-MSGEM
                   ,CURRENT TIMESTAMP
                   ,:TMNFB053.HRECBD-MSGEM
                        :WRK-NULL-HRECBD-MSGEM
                   ,:TMNFB053.HCHEGD-MSGEM
                        :WRK-NULL-HCHEGD-MSGEM)
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS)
           OR (SQLWARN0                EQUAL 'W')
               MOVE 'TETAPA_ESTRT_TRC' TO ERR-DBD-TAB
               MOVE '3154'             TO ERR-LOCAL
           END-IF.

      *----------------------------------------------------------------*
       3154-99-FIM.                    EXIT.
      *----------------------------------------------------------------*
      *----------------------------------------------------------------*
       3155-ETAPA-ESTRT-QUARTA         SECTION.
      *----------------------------------------------------------------*

           EXEC SQL
               INSERT INTO DB2PRD.TETAPA_ESTRT_QUART
                   (NETAPA_MNTRC_ESTRT
                   ,CPTCAO_TBELA
                   ,CMNTRC_TRFGO_ESTRT
                   ,CMNTRC_MSGRA
                   ,CIDTFD_TPO_MSGEM
                   ,HENVIO_MSGEM
                   ,HRECBD_MSGEM
                   ,HCHEGD_MSGEM)
               VALUES
                  ( :WRK-NETAPA-ESTRT
                   ,:WRK-CPTCAO
                   ,:TMNFB053.CMNTRC-TRFGO-ESTRT
                   ,:TMNFB053.CMNTRC-MSGRA
                   ,:TMNFB053.CIDTFD-TPO-MSGEM
                   ,CURRENT TIMESTAMP
                   ,:TMNFB053.HRECBD-MSGEM
                        :WRK-NULL-HRECBD-MSGEM
                   ,:TMNFB053.HCHEGD-MSGEM
                        :WRK-NULL-HCHEGD-MSGEM)
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS)
           OR (SQLWARN0                EQUAL 'W')
               MOVE 'TETAPA_ESTRT_QUART'
                                       TO ERR-DBD-TAB
               MOVE '3155'             TO ERR-LOCAL
           END-IF.

      *----------------------------------------------------------------*
       3155-99-FIM.                    EXIT.
      *----------------------------------------------------------------*
      *----------------------------------------------------------------*
       3156-ETAPA-ESTRT-QUINTA         SECTION.
      *----------------------------------------------------------------*

           EXEC SQL
               INSERT INTO DB2PRD.TETAPA_ESTRT_QTA
                   (NETAPA_MNTRC_ESTRT
                   ,CPTCAO_TBELA
                   ,CMNTRC_TRFGO_ESTRT
                   ,CMNTRC_MSGRA
                   ,CIDTFD_TPO_MSGEM
                   ,HENVIO_MSGEM
                   ,HRECBD_MSGEM
                   ,HCHEGD_MSGEM)
               VALUES
                  ( :WRK-NETAPA-ESTRT
                   ,:WRK-CPTCAO
                   ,:TMNFB053.CMNTRC-TRFGO-ESTRT
                   ,:TMNFB053.CMNTRC-MSGRA
                   ,:TMNFB053.CIDTFD-TPO-MSGEM
                   ,CURRENT TIMESTAMP
                   ,:TMNFB053.HRECBD-MSGEM
                        :WRK-NULL-HRECBD-MSGEM
                   ,:TMNFB053.HCHEGD-MSGEM
                        :WRK-NULL-HCHEGD-MSGEM)
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS)
           OR (SQLWARN0                EQUAL 'W')
               MOVE 'TETAPA_ESTRT_QTA' TO ERR-DBD-TAB
               MOVE '3156'             TO ERR-LOCAL
           END-IF.

      *----------------------------------------------------------------*
       3156-99-FIM.                    EXIT.
      *----------------------------------------------------------------*
      *----------------------------------------------------------------*
       3157-ETAPA-ESTRT-SEXTA          SECTION.
      *----------------------------------------------------------------*

           EXEC SQL
               INSERT INTO DB2PRD.TETAPA_ESTRT_SEXTA
                   (NETAPA_MNTRC_ESTRT
                   ,CPTCAO_TBELA
                   ,CMNTRC_TRFGO_ESTRT
                   ,CMNTRC_MSGRA
                   ,CIDTFD_TPO_MSGEM
                   ,HENVIO_MSGEM
                   ,HRECBD_MSGEM
                   ,HCHEGD_MSGEM)
               VALUES
                  ( :WRK-NETAPA-ESTRT
                   ,:WRK-CPTCAO
                   ,:TMNFB053.CMNTRC-TRFGO-ESTRT
                   ,:TMNFB053.CMNTRC-MSGRA
                   ,:TMNFB053.CIDTFD-TPO-MSGEM
                   ,CURRENT TIMESTAMP
                   ,:TMNFB053.HRECBD-MSGEM
                        :WRK-NULL-HRECBD-MSGEM
                   ,:TMNFB053.HCHEGD-MSGEM
                        :WRK-NULL-HCHEGD-MSGEM)
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS)
           OR (SQLWARN0                EQUAL 'W')
               MOVE 'TETAPA_ESTRT_SEXTA'
                                       TO ERR-DBD-TAB
               MOVE '3157'             TO ERR-LOCAL
           END-IF.

      *----------------------------------------------------------------*
       3157-99-FIM.                    EXIT.
      *----------------------------------------------------------------*
      *----------------------------------------------------------------*
       3158-ETAPA-ESTRT-SABADO         SECTION.
      *----------------------------------------------------------------*

           EXEC SQL
               INSERT INTO DB2PRD.TETAPA_ESTRT_SBADO
                   (NETAPA_MNTRC_ESTRT
                   ,CPTCAO_TBELA
                   ,CMNTRC_TRFGO_ESTRT
                   ,CMNTRC_MSGRA
                   ,CIDTFD_TPO_MSGEM
                   ,HENVIO_MSGEM
                   ,HRECBD_MSGEM
                   ,HCHEGD_MSGEM)
               VALUES
                  ( :WRK-NETAPA-ESTRT
                   ,:WRK-CPTCAO
                   ,:TMNFB053.CMNTRC-TRFGO-ESTRT
                   ,:TMNFB053.CMNTRC-MSGRA
                   ,:TMNFB053.CIDTFD-TPO-MSGEM
                   ,CURRENT TIMESTAMP
                   ,:TMNFB053.HRECBD-MSGEM
                        :WRK-NULL-HRECBD-MSGEM
                   ,:TMNFB053.HCHEGD-MSGEM
                        :WRK-NULL-HCHEGD-MSGEM)
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS)
           OR (SQLWARN0                EQUAL 'W')
               MOVE 'TETAPA_ESTRT_SBADO'
                                       TO ERR-DBD-TAB
               MOVE '3158'             TO ERR-LOCAL
           END-IF.

      *----------------------------------------------------------------*
       3158-99-FIM.                    EXIT.
      *----------------------------------------------------------------*
      *----------------------------------------------------------------*
       3160-EXECUTAR-TRANS-SPBN        SECTION.
      *----------------------------------------------------------------*

           INITIALIZE WRK-NP60-AREA.

           SET INCLUI-LINHA            TO TRUE

           MOVE WRK-NUM-UNICO-TMNF     TO SPBN504-CMNTRC-MSGRA

           MOVE WRK-CSNLZD-MSGRA       TO SPBN504-CSNLZD-MSGRA

           MOVE WRK-CDSTNA-MSGEM       TO SPBN504-CISPB-DSTNA-MSGEM

           MOVE WRK-CEMISR-MSGEM       TO SPBN504-CISPB-EMISR-MSGEM

           MOVE WRK-CTRLIF             TO SPBN504-CCTRL-RQUIS-INSTC

           MOVE WRK-NUOP
                                       TO SPBN504-CUNIC-TRFGO-MSGEM

           MOVE WRK-CFASE-PROCM(WRK-TMNFB029-NREG)
                                       TO SPBN504-CFASE-PROCM.

           MOVE WRK-MSG-SIST           TO SPBN504-CSIST-EXTER.
           MOVE WRK-MSG-EVENTO         TO SPBN504-CEVNTO-MSGEM.
           MOVE WRK-MSG-FLUXO          TO SPBN504-CFLUXO-MSGEM.

           MOVE ZEROS                  TO SPBN504-QMSGEM-RECBD.
           MOVE 1                      TO SPBN504-QMSGEM-ENVID.

           STRING WRK-TIMESTAMP(9:2)
                  '.'
                  WRK-TIMESTAMP(6:2)
                  '.'
                  WRK-TIMESTAMP(1:4)
                  DELIMITED BY SIZE  INTO SPBN504-DINCL-RESUL-MNTRC.

           STRING WRK-TIMESTAMP(12:2)
                  '.'
                  WRK-TIMESTAMP(15:2)
                  '.'
                  WRK-TIMESTAMP(18:2)
                  DELIMITED BY SIZE  INTO SPBN504-HINCL-RESUL-MNTRC.

           MOVE WRK-TIMESTAMP          TO SPBN504-HINCL-RESUL-BACEN.
           MOVE ZEROS                  TO SPBN504-NRELTV-PROCM-RESUL.
           MOVE 1                      TO SPBN504-CIDTFD-ORIGE-ARQ.

           MOVE WRK-PROGRAMA           TO SPBN504-PGM-CHAMADOR.

           MOVE '3160'                 TO SPBN504-LOCAL-CHAMADA.

           EXEC CICS
               START
               TRANSID ('NP60')
               FROM (WRK-NP60-AREA)
               LENGTH  (LENGTH OF WRK-NP60-AREA)
               NOHANDLE
           END-EXEC.

           IF (EIBRESP                 NOT EQUAL DFHRESP(NORMAL))
               MOVE 'CIC'              TO ERR-TIPO-ACESSO
               MOVE DFHEIBLK           TO ERR-DFHEIBLK
               MOVE EIBRESP            TO WRK-ERRO-NP60-EIBRESP
               MOVE WRK-ERRO-NP60      TO ERR-TEXTO
               MOVE 1                  TO ERR-RETURN-CODE
               MOVE 'LOC:0321'         TO ERR-TERMINAL
               PERFORM 9500-GERAR-LOG-ERRO
           END-IF.

      *----------------------------------------------------------------*
       3160-99-EXIT.                   EXIT.
      *----------------------------------------------------------------*
      *----------------------------------------------------------------*
       3200-GRAVAR-ETAPAS             SECTION.
      *----------------------------------------------------------------*

           PERFORM 3210-CARREGAR-ETAPAS.

           IF WRK-MSG-SIST            EQUAL 'DDA'
              GO                      TO 3200-99-FIM
           END-IF.

           EVALUATE TRUE
              WHEN WRK-DOMINGO
                 PERFORM 3232-ETAPA-MNTRC-DOM

              WHEN WRK-SEGUNDA
                 PERFORM 3233-ETAPA-MNTRC-SEG

              WHEN WRK-TERCA
                 PERFORM 3234-ETAPA-MNTRC-TER

              WHEN WRK-QUARTA
                 PERFORM 3235-ETAPA-MNTRC-QUA

              WHEN WRK-QUINTA
                 PERFORM 3236-ETAPA-MNTRC-QUI

              WHEN WRK-SEXTA
                 PERFORM 3237-ETAPA-MNTRC-SEX

              WHEN WRK-SABADO
                 PERFORM 3238-ETAPA-MNTRC-SAB

              WHEN OTHER
                 PERFORM 3231-ETAPA-MNTRC-CONTG
           END-EVALUATE.

           IF SQLCODE                  NOT EQUAL ZEROS
              MOVE WRK-PGM-CHAMADOR    TO ERR-PGM
              MOVE WRK-PROGRAMA        TO ERR-MODULO
              MOVE SQLCODE             TO ERR-SQL-CODE
              MOVE 'DB2'               TO ERR-TIPO-ACESSO
              MOVE SPACES              TO ERR-SEGM
              MOVE SQLCA               TO ERR-SQLCA
              MOVE WRK-PROGRAMA        TO ERR-MODULO
              MOVE 'INSERT'            TO ERR-FUN-COMANDO
              PERFORM 9500-GERAR-LOG-ERRO

              PERFORM 3231-ETAPA-MNTRC-CONTG
           END-IF.

      *----------------------------------------------------------------*
       3200-99-FIM.                    EXIT.
      *----------------------------------------------------------------*
      *----------------------------------------------------------------*
       3210-CARREGAR-ETAPAS            SECTION.
      *----------------------------------------------------------------*
           MOVE WRK-TMNFB029-NREG      TO WRK-S9-02

           PERFORM                     VARYING IND-1
                                       FROM 1
                                       BY 1
                                       UNTIL IND-1
                                       GREATER WRK-9-02
              MOVE WRK-NUM-UNICO-TMNF  TO WRK-CMNTRC-MSGRA
                                         (IND-1)
              MOVE IND-1               TO WRK-NETAPA-MNTRC
                                         (IND-1)
              IF WRK-CFASE-PROCM(IND-1)
                                       NOT NUMERIC
                 MOVE ZEROS            TO WRK-CFASE-PROCM(IND-1)
              END-IF

              IF WRK-SIT-FASE-B29(IND-1)
                                       NOT NUMERIC
                 MOVE 2                TO WRK-SIT-FASE-B29(IND-1)
              END-IF

              IF TMNFWE16-USUARIO      EQUAL LOW-VALUES
              OR TMNFWE16-USUARIO      EQUAL SPACES
                 MOVE -1               TO WRK-CUSUAR-MANUT-N
                                         (IND-1)
                 MOVE SPACES           TO WRK-CUSUAR-MANUT
                                         (IND-1)
              ELSE
                 MOVE ZEROS            TO WRK-CUSUAR-MANUT-N
                                         (IND-1)
                 MOVE TMNFWE16-USUARIO TO WRK-CUSUAR-MANUT
                                         (IND-1)
              END-IF
           END-PERFORM.

      *----------------------------------------------------------------*
       3210-99-FIM.                    EXIT.
      *----------------------------------------------------------------*
      *----------------------------------------------------------------*
       3231-ETAPA-MNTRC-CONTG          SECTION.
      *----------------------------------------------------------------*

           EXEC SQL
                INSERT INTO DB2PRD.TETAPA_MNTRC_CONTG
                       ( CMNTRC_MSGRA
                       , CPTCAO_TBELA
                       , NETAPA_MNTRC_MSGRA
                       , CFASE_PROCM
                       , CIDTFD_SIT_FASE
                       , HINIC_ETAPA_MNTRC
                       , HFNAL_ETAPA_MNTRC
                       , CUSUAR_MANUT
                       , RERRO_ETAPA_MNTRC)
                VALUES ( :WRK-CMNTRC-MSGRA
                       , :WRK-CPTCAO
                       , :WRK-NETAPA-MNTRC
                       , :WRK-CFASE-PROCM
                       , :WRK-SIT-FASE-B29
                       , :WRK-HINIC-MNTRC
                       , :WRK-HFNAL-MNTRC
                       , :WRK-CUSUAR-MANUT
                             :WRK-CUSUAR-MANUT-N
                       , :WRK-RERRO-MNTRC)
                   FOR :WRK-TMNFB029-NREG ROWS
                   NOT ATOMIC CONTINUE ON SQLEXCEPTION
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS)
           OR (SQLWARN0                EQUAL   'W')
               MOVE 'TETAPA_MNTRC_CONTG'
                                       TO ERR-DBD-TAB
               MOVE 'INSERT'           TO ERR-FUN-COMANDO
               MOVE '3231'             TO ERR-LOCAL
               PERFORM 9200-TRATAR-ERRO-DB2
               PERFORM 3300-PROCEDIMENTOS-FINAIS
           END-IF.

      *----------------------------------------------------------------*
       3231-99-FIM.                    EXIT.
      *----------------------------------------------------------------*
      *----------------------------------------------------------------*
       3232-ETAPA-MNTRC-DOM            SECTION.
      *----------------------------------------------------------------*

           EXEC SQL
                INSERT INTO DB2PRD.TETAPA_MNTRC_DMNGO
                       ( CMNTRC_MSGRA
                       , CPTCAO_TBELA
                       , NETAPA_MNTRC_MSGRA
                       , CFASE_PROCM
                       , CIDTFD_SIT_FASE
                       , HINIC_ETAPA_MNTRC
                       , HFNAL_ETAPA_MNTRC
                       , CUSUAR_MANUT
                       , RERRO_ETAPA_MNTRC)
                VALUES ( :WRK-CMNTRC-MSGRA
                       , :WRK-CPTCAO
                       , :WRK-NETAPA-MNTRC
                       , :WRK-CFASE-PROCM
                       , :WRK-SIT-FASE-B29
                       , :WRK-HINIC-MNTRC
                       , :WRK-HFNAL-MNTRC
                       , :WRK-CUSUAR-MANUT
                             :WRK-CUSUAR-MANUT-N
                       , :WRK-RERRO-MNTRC)
                   FOR :WRK-TMNFB029-NREG ROWS
                   NOT ATOMIC CONTINUE ON SQLEXCEPTION
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS)
           OR (SQLWARN0                EQUAL   'W')
               MOVE 'TETAPA_MNTRC_DMNGO'
                                       TO ERR-DBD-TAB
               MOVE '3232'             TO ERR-LOCAL
           END-IF.

      *----------------------------------------------------------------*
       3232-99-FIM.                    EXIT.
      *----------------------------------------------------------------*
      *----------------------------------------------------------------*
       3233-ETAPA-MNTRC-SEG            SECTION.
      *----------------------------------------------------------------*

           EXEC SQL
                INSERT INTO DB2PRD.TETAPA_MNTRC_SEGDA
                       ( CMNTRC_MSGRA
                       , CPTCAO_TBELA
                       , NETAPA_MNTRC_MSGRA
                       , CFASE_PROCM
                       , CIDTFD_SIT_FASE
                       , HINIC_ETAPA_MNTRC
                       , HFNAL_ETAPA_MNTRC
                       , CUSUAR_MANUT
                       , RERRO_ETAPA_MNTRC)
                VALUES ( :WRK-CMNTRC-MSGRA
                       , :WRK-CPTCAO
                       , :WRK-NETAPA-MNTRC
                       , :WRK-CFASE-PROCM
                       , :WRK-SIT-FASE-B29
                       , :WRK-HINIC-MNTRC
                       , :WRK-HFNAL-MNTRC
                       , :WRK-CUSUAR-MANUT
                             :WRK-CUSUAR-MANUT-N
                       , :WRK-RERRO-MNTRC)
                   FOR :WRK-TMNFB029-NREG ROWS
                   NOT ATOMIC CONTINUE ON SQLEXCEPTION
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS)
           OR (SQLWARN0                EQUAL   'W')
               MOVE 'TETAPA_MNTRC_SEGDA'
                                       TO ERR-DBD-TAB
               MOVE '3233'             TO ERR-LOCAL
           END-IF.

      *----------------------------------------------------------------*
       3233-99-FIM.                    EXIT.
      *----------------------------------------------------------------*
      *----------------------------------------------------------------*
       3234-ETAPA-MNTRC-TER            SECTION.
      *----------------------------------------------------------------*

           EXEC SQL
                INSERT INTO DB2PRD.TETAPA_MNTRC_TRC
                       ( CMNTRC_MSGRA
                       , CPTCAO_TBELA
                       , NETAPA_MNTRC_MSGRA
                       , CFASE_PROCM
                       , CIDTFD_SIT_FASE
                       , HINIC_ETAPA_MNTRC
                       , HFNAL_ETAPA_MNTRC
                       , CUSUAR_MANUT
                       , RERRO_ETAPA_MNTRC)
                VALUES ( :WRK-CMNTRC-MSGRA
                       , :WRK-CPTCAO
                       , :WRK-NETAPA-MNTRC
                       , :WRK-CFASE-PROCM
                       , :WRK-SIT-FASE-B29
                       , :WRK-HINIC-MNTRC
                       , :WRK-HFNAL-MNTRC
                       , :WRK-CUSUAR-MANUT
                             :WRK-CUSUAR-MANUT-N
                       , :WRK-RERRO-MNTRC)
                   FOR :WRK-TMNFB029-NREG ROWS
                   NOT ATOMIC CONTINUE ON SQLEXCEPTION
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS)
           OR (SQLWARN0                EQUAL   'W')
               MOVE 'TETAPA_MNTRC_TRC' TO ERR-DBD-TAB
               MOVE '3234'             TO ERR-LOCAL
           END-IF.

      *----------------------------------------------------------------*
       3234-99-FIM.                    EXIT.
      *----------------------------------------------------------------*
      *----------------------------------------------------------------*
       3235-ETAPA-MNTRC-QUA            SECTION.
      *----------------------------------------------------------------*

           EXEC SQL
                INSERT INTO DB2PRD.TETAPA_MNTRC_QUART
                       ( CMNTRC_MSGRA
                       , CPTCAO_TBELA
                       , NETAPA_MNTRC_MSGRA
                       , CFASE_PROCM
                       , CIDTFD_SIT_FASE
                       , HINIC_ETAPA_MNTRC
                       , HFNAL_ETAPA_MNTRC
                       , CUSUAR_MANUT
                       , RERRO_ETAPA_MNTRC)
                VALUES ( :WRK-CMNTRC-MSGRA
                       , :WRK-CPTCAO
                       , :WRK-NETAPA-MNTRC
                       , :WRK-CFASE-PROCM
                       , :WRK-SIT-FASE-B29
                       , :WRK-HINIC-MNTRC
                       , :WRK-HFNAL-MNTRC
                       , :WRK-CUSUAR-MANUT
                             :WRK-CUSUAR-MANUT-N
                       , :WRK-RERRO-MNTRC)
                   FOR :WRK-TMNFB029-NREG ROWS
                   NOT ATOMIC CONTINUE ON SQLEXCEPTION
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS)
           OR (SQLWARN0                EQUAL   'W')
               MOVE 'TETAPA_MNTRC_QUART'
                                       TO ERR-DBD-TAB
               MOVE '3235'             TO ERR-LOCAL
           END-IF.

      *----------------------------------------------------------------*
       3235-99-FIM.                    EXIT.
      *----------------------------------------------------------------*
      *----------------------------------------------------------------*
       3236-ETAPA-MNTRC-QUI            SECTION.
      *----------------------------------------------------------------*

           EXEC SQL
                INSERT INTO DB2PRD.TETAPA_MNTRC_QTA
                       ( CMNTRC_MSGRA
                       , CPTCAO_TBELA
                       , NETAPA_MNTRC_MSGRA
                       , CFASE_PROCM
                       , CIDTFD_SIT_FASE
                       , HINIC_ETAPA_MNTRC
                       , HFNAL_ETAPA_MNTRC
                       , CUSUAR_MANUT
                       , RERRO_ETAPA_MNTRC)
                VALUES ( :WRK-CMNTRC-MSGRA
                       , :WRK-CPTCAO
                       , :WRK-NETAPA-MNTRC
                       , :WRK-CFASE-PROCM
                       , :WRK-SIT-FASE-B29
                       , :WRK-HINIC-MNTRC
                       , :WRK-HFNAL-MNTRC
                       , :WRK-CUSUAR-MANUT
                             :WRK-CUSUAR-MANUT-N
                       , :WRK-RERRO-MNTRC)
                   FOR :WRK-TMNFB029-NREG ROWS
                   NOT ATOMIC CONTINUE ON SQLEXCEPTION
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS)
           OR (SQLWARN0                EQUAL   'W')
               MOVE 'TETAPA_MNTRC_QTA' TO ERR-DBD-TAB
               MOVE '3236'             TO ERR-LOCAL
           END-IF.

      *----------------------------------------------------------------*
       3236-99-FIM.                    EXIT.
      *----------------------------------------------------------------*
      *----------------------------------------------------------------*
       3237-ETAPA-MNTRC-SEX            SECTION.
      *----------------------------------------------------------------*

           EXEC SQL
                INSERT INTO DB2PRD.TETAPA_MNTRC_SEXTA
                       ( CMNTRC_MSGRA
                       , CPTCAO_TBELA
                       , NETAPA_MNTRC_MSGRA
                       , CFASE_PROCM
                       , CIDTFD_SIT_FASE
                       , HINIC_ETAPA_MNTRC
                       , HFNAL_ETAPA_MNTRC
                       , CUSUAR_MANUT
                       , RERRO_ETAPA_MNTRC)
                VALUES ( :WRK-CMNTRC-MSGRA
                       , :WRK-CPTCAO
                       , :WRK-NETAPA-MNTRC
                       , :WRK-CFASE-PROCM
                       , :WRK-SIT-FASE-B29
                       , :WRK-HINIC-MNTRC
                       , :WRK-HFNAL-MNTRC
                       , :WRK-CUSUAR-MANUT
                             :WRK-CUSUAR-MANUT-N
                       , :WRK-RERRO-MNTRC)
                   FOR :WRK-TMNFB029-NREG ROWS
                   NOT ATOMIC CONTINUE ON SQLEXCEPTION
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS)
           OR (SQLWARN0                EQUAL   'W')
               MOVE 'TETAPA_MNTRC_SEXTA'
                                       TO ERR-DBD-TAB
               MOVE '3237'             TO ERR-LOCAL
           END-IF.

      *----------------------------------------------------------------*
       3237-99-FIM.                    EXIT.
      *----------------------------------------------------------------*
      *----------------------------------------------------------------*
       3238-ETAPA-MNTRC-SAB            SECTION.
      *----------------------------------------------------------------*

           EXEC SQL
                INSERT INTO DB2PRD.TETAPA_MNTRC_SBADO
                       ( CMNTRC_MSGRA
                       , CPTCAO_TBELA
                       , NETAPA_MNTRC_MSGRA
                       , CFASE_PROCM
                       , CIDTFD_SIT_FASE
                       , HINIC_ETAPA_MNTRC
                       , HFNAL_ETAPA_MNTRC
                       , CUSUAR_MANUT
                       , RERRO_ETAPA_MNTRC)
                VALUES ( :WRK-CMNTRC-MSGRA
                       , :WRK-CPTCAO
                       , :WRK-NETAPA-MNTRC
                       , :WRK-CFASE-PROCM
                       , :WRK-SIT-FASE-B29
                       , :WRK-HINIC-MNTRC
                       , :WRK-HFNAL-MNTRC
                       , :WRK-CUSUAR-MANUT
                             :WRK-CUSUAR-MANUT-N
                       , :WRK-RERRO-MNTRC)
                   FOR :WRK-TMNFB029-NREG ROWS
                   NOT ATOMIC CONTINUE ON SQLEXCEPTION
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS)
           OR (SQLWARN0                EQUAL   'W')
               MOVE 'TETAPA_MNTRC_SBADO'
                                       TO ERR-DBD-TAB
               MOVE '3238'             TO ERR-LOCAL
           END-IF.

      *----------------------------------------------------------------*
       3238-99-FIM.                    EXIT.
      *----------------------------------------------------------------*
      *----------------------------------------------------------------*
       3300-PROCEDIMENTOS-FINAIS       SECTION.
      *----------------------------------------------------------------*

           MOVE ERRO-AREA              TO TMNFWE09-ERRO-AREA
                                       OF DFHCOMMAREA

           IF  WRK-FILA-CAMARA         NOT EQUAL LOW-VALUES
           AND WRK-FILA-CAMARA         NOT EQUAL SPACES
              MOVE WRK-FILA-CAMARA     TO WRK-MQ-FILA
              MOVE WRK-HAND-CAMARA     TO WRK-HANDLE-FILA
              PERFORM 2950-FECHAR-FILA
           END-IF

           IF  WRK-FILA-RETORNO        NOT EQUAL LOW-VALUES
           AND WRK-FILA-RETORNO        NOT EQUAL SPACES
              MOVE WRK-FILA-RETORNO    TO WRK-MQ-FILA
              MOVE WRK-HAND-RETORNO    TO WRK-HANDLE-FILA
              PERFORM 2950-FECHAR-FILA
           END-IF

           IF  ERR-RETURN-CODE         EQUAL ZEROS
           AND WRK-CTRLIF(5:4)         NOT EQUAL 'CARG'
030217         PERFORM 7000-GRAVAR-TAB-TEMP
           END-IF.

           EXEC CICS RETURN            END-EXEC.

      *----------------------------------------------------------------*
       3300-99-FIM.                    EXIT.
      *----------------------------------------------------------------*
      *----------------------------------------------------------------*
       3400-INCLUIR-MNTRC-NOVA         SECTION.
      *----------------------------------------------------------------*

           INITIALIZE WRK-MNTRC-HOST.

           MOVE 'N'                    TO WRK-BASE-CONTG

           MOVE WRK-NUM-UNICO-TMNF     TO WRK-HOST-CMNTRC-MSGRA

           MOVE WRK-CSNLZD-MSGRA       TO WRK-HOST-CSNLZD-MSGRA

           MOVE WRK-CDSTNA-MSGEM       TO WRK-HOST-CISPB-DSTNA

           MOVE WRK-CEMISR-MSGEM       TO WRK-HOST-CISPB-EMISR

           IF  TMNFWE16-ENTRADA-MANUAL EQUAL 'S'
               MOVE WRK-ENVIO-MANUAL   TO WRK-HOST-CINDCD-ORIGE
           ELSE
               MOVE WRK-ENVIO-LEGADO   TO WRK-HOST-CINDCD-ORIGE
           END-IF.

           MOVE WRK-MF                 TO WRK-HOST-CINDCD-PLATF

           MOVE TMNFWE16-HOCORR-WKFLW  TO WRK-HOST-HMOVTO-INFO
                                          WRK-HOST-HPROCM-MSGEM
                                          WRK-HOST-HULT-ATULZ

           MOVE WRK-MSG-SIST           TO WRK-HOST-CSIST-EXTER
           MOVE WRK-MSG-EVENTO         TO WRK-HOST-CEVNTO-MSGEM
           MOVE WRK-MSG-FLUXO          TO WRK-HOST-CFLUXO-MSGEM

           MOVE WRK-CINDCD-RPROC-AUX   TO WRK-HOST-CINDCD-RPROC
           MOVE -1                     TO WRK-NULL-06

REPROC     IF  WRK-RPROC-CONVERSAO
.                                      EQUAL 'S'
.              MOVE ZEROS              TO WRK-NULL-06
.              PERFORM 6000-MOVER-RPROC-TXT
.          END-IF.
.
.          IF  WRK-RPROC-CRIPTOGRAFIA
.                                      EQUAL 'S'
.              MOVE ZEROS              TO WRK-NULL-06
.              PERFORM 6100-MOVER-RPROC-XML
.          END-IF.
.
.          IF  WRK-RPROC-ENVIO-BACEN
.                                      EQUAL 'S'
.              MOVE ZEROS              TO WRK-NULL-06
.              PERFORM 6200-MOVER-RPROC-CRIPTO
REPROC     END-IF.

           MOVE TMNFWE16-CDOM-MSGRA-SPB
                                       TO WRK-HOST-CDOMNO-MSGRA

           MOVE WRK-CTRLIF             TO WRK-HOST-CCTRL-INSTC

           IF WRK-MSG-PROBLEMA         EQUAL 'S'
              MOVE SPACES              TO WRK-HOST-CCTRL-INSTC
           END-IF.

           MOVE WRK-NUOP               TO WRK-HOST-CUNIC-TRFGO
           MOVE TMNFWE04-MQID-48       TO WRK-HOST-RIDTFD-MSGEM

           IF WRK-MSG-SIST             EQUAL 'DDA'
              MOVE WRK-CFASE-PROCM(WRK-TMNFB029-NREG)
                                       TO WRK-HOST-CFASE-PROCM
              MOVE WRK-SIT-FASE-B29(WRK-TMNFB029-NREG)
                                       TO WRK-HOST-SIT-FASE
              MOVE WRK-HINIC-MNTRC(1)  TO WRK-HOST-HINIC-ETAPA
              MOVE WRK-HFNAL-MNTRC(WRK-TMNFB029-NREG)
                                       TO WRK-HOST-HFNAL-ETAPA
              MOVE WRK-RERRO-MNTRC(WRK-TMNFB029-NREG)
                                       TO WRK-HOST-RERRO-ETAPA

              MOVE ZEROS               TO WRK-NULL-01
                                          WRK-NULL-02
                                          WRK-NULL-03
                                          WRK-NULL-04
                                          WRK-NULL-05

           ELSE
              MOVE ZEROS               TO WRK-HOST-CFASE-PROCM
                                          WRK-HOST-SIT-FASE

              MOVE SPACES              TO WRK-HOST-HINIC-ETAPA
                                          WRK-HOST-HFNAL-ETAPA
                                          WRK-HOST-RERRO-ETAPA

              MOVE -1                  TO WRK-NULL-01
                                          WRK-NULL-02
                                          WRK-NULL-03
                                          WRK-NULL-04
                                          WRK-NULL-05
           END-IF.

           IF  WRK-MSG-TXT-LENGTH      GREATER ZEROS
               MOVE ZEROS              TO WRK-NULL-07
           ELSE
               MOVE -1                 TO WRK-NULL-07
           END-IF.

           IF  WRK-MSG-XML-LENGTH      GREATER ZEROS
               MOVE ZEROS              TO WRK-NULL-08
           ELSE
               MOVE -1                 TO WRK-NULL-08
           END-IF.

           EVALUATE TRUE
              WHEN WRK-DOMINGO
                 PERFORM 3410-INCLUIR-MNTRC-DOMINGO

              WHEN WRK-SEGUNDA
                 PERFORM 3420-INCLUIR-MNTRC-SEGUNDA

              WHEN WRK-TERCA
                 PERFORM 3430-INCLUIR-MNTRC-TERCA

              WHEN WRK-QUARTA
                 PERFORM 3440-INCLUIR-MNTRC-QUARTA

              WHEN WRK-QUINTA
                 PERFORM 3450-INCLUIR-MNTRC-QUINTA

              WHEN WRK-SEXTA
                 PERFORM 3460-INCLUIR-MNTRC-SEXTA

              WHEN WRK-SABADO
                 PERFORM 3470-INCLUIR-MNTRC-SABADO

              WHEN OTHER
                 PERFORM 3480-INCLUIR-MNTRC-CONTG
           END-EVALUATE.

           IF SQLCODE                  NOT EQUAL ZEROS
              MOVE WRK-PGM-CHAMADOR    TO ERR-PGM
              MOVE WRK-PROGRAMA        TO ERR-MODULO
              MOVE SQLCODE             TO ERR-SQL-CODE
              MOVE 'DB2'               TO ERR-TIPO-ACESSO
              MOVE SPACES              TO ERR-SEGM
              MOVE SQLCA               TO ERR-SQLCA
              MOVE WRK-PROGRAMA        TO ERR-MODULO
              MOVE 'INSERT'            TO ERR-FUN-COMANDO
              PERFORM 9500-GERAR-LOG-ERRO

              PERFORM 3480-INCLUIR-MNTRC-CONTG
           END-IF.

      *----------------------------------------------------------------*
       3400-99-FIM.                    EXIT.
      *----------------------------------------------------------------*
      *----------------------------------------------------------------*
       3410-INCLUIR-MNTRC-DOMINGO      SECTION.
      *----------------------------------------------------------------*

           EXEC SQL
              INSERT INTO DB2PRD.TMNTRC_MSGRA_DMNGO
                     ( CMNTRC_MSGRA
                     , CPTCAO_TBELA
                     , CSNLZD_MSGRA
                     , CINDCD_ORIGE_TRFGO
                     , CINDCD_ORIGE_PLATF
                     , CIDTFD_PROT_ARQ
                     , HPROCM_MSGEM
                     , HULT_ATULZ
                     , HMOVTO_INFO_MSGEM
                     , HCONF_ENVIO_MSGEM
                     , HCONF_LEITU_MSGEM
                     , CISPB_DSTNA_MSGEM
                     , CISPB_EMISR_MSGEM
                     , CCTRL_RQUIS_INSTC
                     , CUNIC_TRFGO_MSGEM
                     , RIDTFD_MSGEM
                     , NMNTRC_TRFGO_MSGEM
                     , CINDCD_RPROC_MSGEM
                     , CDOMNO_MSGRA_SIST
                     , CSIST_EXTER
                     , CEVNTO_MSGEM
                     , CFLUXO_MSGEM
                     , CFASE_PROCM
                     , CIDTFD_SIT_FASE
                     , HINIC_ETAPA_MNTRC
                     , HFNAL_ETAPA_MNTRC
                     , RERRO_ETAPA_MNTRC
                     , WCONTD_MSGEM_TEXTO
                     , WCONTD_MSGEM_XML
                     , WCONTD_MSGEM_RPROC )
              VALUES ( :WRK-HOST-CMNTRC-MSGRA
                     , :WRK-CPTCAO
                     , :WRK-HOST-CSNLZD-MSGRA
                     , :WRK-HOST-CINDCD-ORIGE
                     , :WRK-HOST-CINDCD-PLATF
                     , NULL
                     , :WRK-HOST-HPROCM-MSGEM
                     , :WRK-HOST-HULT-ATULZ
                     , :WRK-HOST-HMOVTO-INFO
                     , NULL
                     , NULL
                     , :WRK-HOST-CISPB-DSTNA
                     , :WRK-HOST-CISPB-EMISR
                     , :WRK-HOST-CCTRL-INSTC
                     , :WRK-HOST-CUNIC-TRFGO
                     , :WRK-HOST-RIDTFD-MSGEM
                     , NULL
                     , :WRK-HOST-CINDCD-RPROC
                     , :WRK-HOST-CDOMNO-MSGRA
                     , :WRK-HOST-CSIST-EXTER
                     , :WRK-HOST-CEVNTO-MSGEM
                     , :WRK-HOST-CFLUXO-MSGEM
                     , :WRK-HOST-CFASE-PROCM
                            :WRK-NULL-01
                     , :WRK-HOST-SIT-FASE
                            :WRK-NULL-02
                     , :WRK-HOST-HINIC-ETAPA
                            :WRK-NULL-03
                     , :WRK-HOST-HFNAL-ETAPA
                            :WRK-NULL-04
                     , :WRK-HOST-RERRO-ETAPA
                            :WRK-NULL-05
                     , :WRK-MSG-TXT
                            :WRK-NULL-07
                     , :WRK-MSG-XML
                            :WRK-NULL-08
                     , :WRK-MSG-A-ENVIAR
                          :WRK-NULL-06)
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS)
           OR (SQLWARN0                EQUAL   'W')
               MOVE 'TMNTRC_MSGRA_DMNGO'
                                       TO ERR-DBD-TAB
               MOVE '3410'             TO ERR-LOCAL
           END-IF.

      *----------------------------------------------------------------*
       3410-99-FIM.                    EXIT.
      *----------------------------------------------------------------*
      *----------------------------------------------------------------*
       3420-INCLUIR-MNTRC-SEGUNDA      SECTION.
      *----------------------------------------------------------------*

           EXEC SQL
              INSERT INTO DB2PRD.TMNTRC_MSGRA_SEGDA
                     ( CMNTRC_MSGRA
                     , CPTCAO_TBELA
                     , CSNLZD_MSGRA
                     , CINDCD_ORIGE_TRFGO
                     , CINDCD_ORIGE_PLATF
                     , CIDTFD_PROT_ARQ
                     , HPROCM_MSGEM
                     , HULT_ATULZ
                     , HMOVTO_INFO_MSGEM
                     , HCONF_ENVIO_MSGEM
                     , HCONF_LEITU_MSGEM
                     , CISPB_DSTNA_MSGEM
                     , CISPB_EMISR_MSGEM
                     , CCTRL_RQUIS_INSTC
                     , CUNIC_TRFGO_MSGEM
                     , RIDTFD_MSGEM
                     , NMNTRC_TRFGO_MSGEM
                     , CINDCD_RPROC_MSGEM
                     , CDOMNO_MSGRA_SIST
                     , CSIST_EXTER
                     , CEVNTO_MSGEM
                     , CFLUXO_MSGEM
                     , CFASE_PROCM
                     , CIDTFD_SIT_FASE
                     , HINIC_ETAPA_MNTRC
                     , HFNAL_ETAPA_MNTRC
                     , RERRO_ETAPA_MNTRC
                     , WCONTD_MSGEM_TEXTO
                     , WCONTD_MSGEM_XML
                     , WCONTD_MSGEM_RPROC )
              VALUES ( :WRK-HOST-CMNTRC-MSGRA
                     , :WRK-CPTCAO
                     , :WRK-HOST-CSNLZD-MSGRA
                     , :WRK-HOST-CINDCD-ORIGE
                     , :WRK-HOST-CINDCD-PLATF
                     , NULL
                     , :WRK-HOST-HPROCM-MSGEM
                     , :WRK-HOST-HULT-ATULZ
                     , :WRK-HOST-HMOVTO-INFO
                     , NULL
                     , NULL
                     , :WRK-HOST-CISPB-DSTNA
                     , :WRK-HOST-CISPB-EMISR
                     , :WRK-HOST-CCTRL-INSTC
                     , :WRK-HOST-CUNIC-TRFGO
                     , :WRK-HOST-RIDTFD-MSGEM
                     , NULL
                     , :WRK-HOST-CINDCD-RPROC
                     , :WRK-HOST-CDOMNO-MSGRA
                     , :WRK-HOST-CSIST-EXTER
                     , :WRK-HOST-CEVNTO-MSGEM
                     , :WRK-HOST-CFLUXO-MSGEM
                     , :WRK-HOST-CFASE-PROCM
                            :WRK-NULL-01
                     , :WRK-HOST-SIT-FASE
                            :WRK-NULL-02
                     , :WRK-HOST-HINIC-ETAPA
                            :WRK-NULL-03
                     , :WRK-HOST-HFNAL-ETAPA
                            :WRK-NULL-04
                     , :WRK-HOST-RERRO-ETAPA
                            :WRK-NULL-05
                     , :WRK-MSG-TXT
                            :WRK-NULL-07
                     , :WRK-MSG-XML
                            :WRK-NULL-08
                     , :WRK-MSG-A-ENVIAR
                          :WRK-NULL-06)
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS)
           OR (SQLWARN0                EQUAL   'W')
               MOVE 'TMNTRC_MSGRA_SEGDA'
                                       TO ERR-DBD-TAB
               MOVE '3420'             TO ERR-LOCAL
           END-IF.

      *----------------------------------------------------------------*
       3420-99-FIM.                    EXIT.
      *----------------------------------------------------------------*
      *----------------------------------------------------------------*
       3430-INCLUIR-MNTRC-TERCA       SECTION.
      *----------------------------------------------------------------*

           EXEC SQL
              INSERT INTO DB2PRD.TMNTRC_MSGRA_TRC
                     ( CMNTRC_MSGRA
                     , CPTCAO_TBELA
                     , CSNLZD_MSGRA
                     , CINDCD_ORIGE_TRFGO
                     , CINDCD_ORIGE_PLATF
                     , CIDTFD_PROT_ARQ
                     , HPROCM_MSGEM
                     , HULT_ATULZ
                     , HMOVTO_INFO_MSGEM
                     , HCONF_ENVIO_MSGEM
                     , HCONF_LEITU_MSGEM
                     , CISPB_DSTNA_MSGEM
                     , CISPB_EMISR_MSGEM
                     , CCTRL_RQUIS_INSTC
                     , CUNIC_TRFGO_MSGEM
                     , RIDTFD_MSGEM
                     , NMNTRC_TRFGO_MSGEM
                     , CINDCD_RPROC_MSGEM
                     , CDOMNO_MSGRA_SIST
                     , CSIST_EXTER
                     , CEVNTO_MSGEM
                     , CFLUXO_MSGEM
                     , CFASE_PROCM
                     , CIDTFD_SIT_FASE
                     , HINIC_ETAPA_MNTRC
                     , HFNAL_ETAPA_MNTRC
                     , RERRO_ETAPA_MNTRC
                     , WCONTD_MSGEM_TEXTO
                     , WCONTD_MSGEM_XML
                     , WCONTD_MSGEM_RPROC )
              VALUES ( :WRK-HOST-CMNTRC-MSGRA
                     , :WRK-CPTCAO
                     , :WRK-HOST-CSNLZD-MSGRA
                     , :WRK-HOST-CINDCD-ORIGE
                     , :WRK-HOST-CINDCD-PLATF
                     , NULL
                     , :WRK-HOST-HPROCM-MSGEM
                     , :WRK-HOST-HULT-ATULZ
                     , :WRK-HOST-HMOVTO-INFO
                     , NULL
                     , NULL
                     , :WRK-HOST-CISPB-DSTNA
                     , :WRK-HOST-CISPB-EMISR
                     , :WRK-HOST-CCTRL-INSTC
                     , :WRK-HOST-CUNIC-TRFGO
                     , :WRK-HOST-RIDTFD-MSGEM
                     , NULL
                     , :WRK-HOST-CINDCD-RPROC
                     , :WRK-HOST-CDOMNO-MSGRA
                     , :WRK-HOST-CSIST-EXTER
                     , :WRK-HOST-CEVNTO-MSGEM
                     , :WRK-HOST-CFLUXO-MSGEM
                     , :WRK-HOST-CFASE-PROCM
                            :WRK-NULL-01
                     , :WRK-HOST-SIT-FASE
                            :WRK-NULL-02
                     , :WRK-HOST-HINIC-ETAPA
                            :WRK-NULL-03
                     , :WRK-HOST-HFNAL-ETAPA
                            :WRK-NULL-04
                     , :WRK-HOST-RERRO-ETAPA
                            :WRK-NULL-05
                     , :WRK-MSG-TXT
                            :WRK-NULL-07
                     , :WRK-MSG-XML
                            :WRK-NULL-08
                     , :WRK-MSG-A-ENVIAR
                          :WRK-NULL-06)
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS)
           OR (SQLWARN0                EQUAL   'W')
               MOVE 'TMNTRC_MSGRA_TRC' TO ERR-DBD-TAB
               MOVE '3430'             TO ERR-LOCAL
           END-IF.

      *----------------------------------------------------------------*
       3430-99-FIM.                    EXIT.
      *----------------------------------------------------------------*
      *----------------------------------------------------------------*
       3440-INCLUIR-MNTRC-QUARTA       SECTION.
      *----------------------------------------------------------------*

           EXEC SQL
              INSERT INTO DB2PRD.TMNTRC_MSGRA_QUART
                     ( CMNTRC_MSGRA
                     , CPTCAO_TBELA
                     , CSNLZD_MSGRA
                     , CINDCD_ORIGE_TRFGO
                     , CINDCD_ORIGE_PLATF
                     , CIDTFD_PROT_ARQ
                     , HPROCM_MSGEM
                     , HULT_ATULZ
                     , HMOVTO_INFO_MSGEM
                     , HCONF_ENVIO_MSGEM
                     , HCONF_LEITU_MSGEM
                     , CISPB_DSTNA_MSGEM
                     , CISPB_EMISR_MSGEM
                     , CCTRL_RQUIS_INSTC
                     , CUNIC_TRFGO_MSGEM
                     , RIDTFD_MSGEM
                     , NMNTRC_TRFGO_MSGEM
                     , CINDCD_RPROC_MSGEM
                     , CDOMNO_MSGRA_SIST
                     , CSIST_EXTER
                     , CEVNTO_MSGEM
                     , CFLUXO_MSGEM
                     , CFASE_PROCM
                     , CIDTFD_SIT_FASE
                     , HINIC_ETAPA_MNTRC
                     , HFNAL_ETAPA_MNTRC
                     , RERRO_ETAPA_MNTRC
                     , WCONTD_MSGEM_TEXTO
                     , WCONTD_MSGEM_XML
                     , WCONTD_MSGEM_RPROC )
              VALUES ( :WRK-HOST-CMNTRC-MSGRA
                     , :WRK-CPTCAO
                     , :WRK-HOST-CSNLZD-MSGRA
                     , :WRK-HOST-CINDCD-ORIGE
                     , :WRK-HOST-CINDCD-PLATF
                     , NULL
                     , :WRK-HOST-HPROCM-MSGEM
                     , :WRK-HOST-HULT-ATULZ
                     , :WRK-HOST-HMOVTO-INFO
                     , NULL
                     , NULL
                     , :WRK-HOST-CISPB-DSTNA
                     , :WRK-HOST-CISPB-EMISR
                     , :WRK-HOST-CCTRL-INSTC
                     , :WRK-HOST-CUNIC-TRFGO
                     , :WRK-HOST-RIDTFD-MSGEM
                     , NULL
                     , :WRK-HOST-CINDCD-RPROC
                     , :WRK-HOST-CDOMNO-MSGRA
                     , :WRK-HOST-CSIST-EXTER
                     , :WRK-HOST-CEVNTO-MSGEM
                     , :WRK-HOST-CFLUXO-MSGEM
                     , :WRK-HOST-CFASE-PROCM
                            :WRK-NULL-01
                     , :WRK-HOST-SIT-FASE
                            :WRK-NULL-02
                     , :WRK-HOST-HINIC-ETAPA
                            :WRK-NULL-03
                     , :WRK-HOST-HFNAL-ETAPA
                            :WRK-NULL-04
                     , :WRK-HOST-RERRO-ETAPA
                            :WRK-NULL-05
                     , :WRK-MSG-TXT
                            :WRK-NULL-07
                     , :WRK-MSG-XML
                            :WRK-NULL-08
                     , :WRK-MSG-A-ENVIAR
                          :WRK-NULL-06)
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS)
           OR (SQLWARN0                EQUAL   'W')
               MOVE 'TMNTRC_MSGRA_QUART'
                                       TO ERR-DBD-TAB
               MOVE '3440'             TO ERR-LOCAL
           END-IF.

      *----------------------------------------------------------------*
       3440-99-FIM.                    EXIT.
      *----------------------------------------------------------------*
      *----------------------------------------------------------------*
       3450-INCLUIR-MNTRC-QUINTA       SECTION.
      *----------------------------------------------------------------*

           EXEC SQL
              INSERT INTO DB2PRD.TMNTRC_MSGRA_QTA
                     ( CMNTRC_MSGRA
                     , CPTCAO_TBELA
                     , CSNLZD_MSGRA
                     , CINDCD_ORIGE_TRFGO
                     , CINDCD_ORIGE_PLATF
                     , CIDTFD_PROT_ARQ
                     , HPROCM_MSGEM
                     , HULT_ATULZ
                     , HMOVTO_INFO_MSGEM
                     , HCONF_ENVIO_MSGEM
                     , HCONF_LEITU_MSGEM
                     , CISPB_DSTNA_MSGEM
                     , CISPB_EMISR_MSGEM
                     , CCTRL_RQUIS_INSTC
                     , CUNIC_TRFGO_MSGEM
                     , RIDTFD_MSGEM
                     , NMNTRC_TRFGO_MSGEM
                     , CINDCD_RPROC_MSGEM
                     , CDOMNO_MSGRA_SIST
                     , CSIST_EXTER
                     , CEVNTO_MSGEM
                     , CFLUXO_MSGEM
                     , CFASE_PROCM
                     , CIDTFD_SIT_FASE
                     , HINIC_ETAPA_MNTRC
                     , HFNAL_ETAPA_MNTRC
                     , RERRO_ETAPA_MNTRC
                     , WCONTD_MSGEM_TEXTO
                     , WCONTD_MSGEM_XML
                     , WCONTD_MSGEM_RPROC )
              VALUES ( :WRK-HOST-CMNTRC-MSGRA
                     , :WRK-CPTCAO
                     , :WRK-HOST-CSNLZD-MSGRA
                     , :WRK-HOST-CINDCD-ORIGE
                     , :WRK-HOST-CINDCD-PLATF
                     , NULL
                     , :WRK-HOST-HPROCM-MSGEM
                     , :WRK-HOST-HULT-ATULZ
                     , :WRK-HOST-HMOVTO-INFO
                     , NULL
                     , NULL
                     , :WRK-HOST-CISPB-DSTNA
                     , :WRK-HOST-CISPB-EMISR
                     , :WRK-HOST-CCTRL-INSTC
                     , :WRK-HOST-CUNIC-TRFGO
                     , :WRK-HOST-RIDTFD-MSGEM
                     , NULL
                     , :WRK-HOST-CINDCD-RPROC
                     , :WRK-HOST-CDOMNO-MSGRA
                     , :WRK-HOST-CSIST-EXTER
                     , :WRK-HOST-CEVNTO-MSGEM
                     , :WRK-HOST-CFLUXO-MSGEM
                     , :WRK-HOST-CFASE-PROCM
                            :WRK-NULL-01
                     , :WRK-HOST-SIT-FASE
                            :WRK-NULL-02
                     , :WRK-HOST-HINIC-ETAPA
                            :WRK-NULL-03
                     , :WRK-HOST-HFNAL-ETAPA
                            :WRK-NULL-04
                     , :WRK-HOST-RERRO-ETAPA
                            :WRK-NULL-05
                     , :WRK-MSG-TXT
                            :WRK-NULL-07
                     , :WRK-MSG-XML
                            :WRK-NULL-08
                     , :WRK-MSG-A-ENVIAR
                          :WRK-NULL-06)
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS)
           OR (SQLWARN0                EQUAL   'W')
               MOVE 'TMNTRC_MSGRA_QTA'
                                       TO ERR-DBD-TAB
               MOVE '3450'             TO ERR-LOCAL
           END-IF.

      *----------------------------------------------------------------*
       3450-99-FIM.                    EXIT.
      *----------------------------------------------------------------*
      *----------------------------------------------------------------*
       3460-INCLUIR-MNTRC-SEXTA        SECTION.
      *----------------------------------------------------------------*

           EXEC SQL
              INSERT INTO DB2PRD.TMNTRC_MSGRA_SEXTA
                     ( CMNTRC_MSGRA
                     , CPTCAO_TBELA
                     , CSNLZD_MSGRA
                     , CINDCD_ORIGE_TRFGO
                     , CINDCD_ORIGE_PLATF
                     , CIDTFD_PROT_ARQ
                     , HPROCM_MSGEM
                     , HULT_ATULZ
                     , HMOVTO_INFO_MSGEM
                     , HCONF_ENVIO_MSGEM
                     , HCONF_LEITU_MSGEM
                     , CISPB_DSTNA_MSGEM
                     , CISPB_EMISR_MSGEM
                     , CCTRL_RQUIS_INSTC
                     , CUNIC_TRFGO_MSGEM
                     , RIDTFD_MSGEM
                     , NMNTRC_TRFGO_MSGEM
                     , CINDCD_RPROC_MSGEM
                     , CDOMNO_MSGRA_SIST
                     , CSIST_EXTER
                     , CEVNTO_MSGEM
                     , CFLUXO_MSGEM
                     , CFASE_PROCM
                     , CIDTFD_SIT_FASE
                     , HINIC_ETAPA_MNTRC
                     , HFNAL_ETAPA_MNTRC
                     , RERRO_ETAPA_MNTRC
                     , WCONTD_MSGEM_TEXTO
                     , WCONTD_MSGEM_XML
                     , WCONTD_MSGEM_RPROC )
              VALUES ( :WRK-HOST-CMNTRC-MSGRA
                     , :WRK-CPTCAO
                     , :WRK-HOST-CSNLZD-MSGRA
                     , :WRK-HOST-CINDCD-ORIGE
                     , :WRK-HOST-CINDCD-PLATF
                     , NULL
                     , :WRK-HOST-HPROCM-MSGEM
                     , :WRK-HOST-HULT-ATULZ
                     , :WRK-HOST-HMOVTO-INFO
                     , NULL
                     , NULL
                     , :WRK-HOST-CISPB-DSTNA
                     , :WRK-HOST-CISPB-EMISR
                     , :WRK-HOST-CCTRL-INSTC
                     , :WRK-HOST-CUNIC-TRFGO
                     , :WRK-HOST-RIDTFD-MSGEM
                     , NULL
                     , :WRK-HOST-CINDCD-RPROC
                     , :WRK-HOST-CDOMNO-MSGRA
                     , :WRK-HOST-CSIST-EXTER
                     , :WRK-HOST-CEVNTO-MSGEM
                     , :WRK-HOST-CFLUXO-MSGEM
                     , :WRK-HOST-CFASE-PROCM
                            :WRK-NULL-01
                     , :WRK-HOST-SIT-FASE
                            :WRK-NULL-02
                     , :WRK-HOST-HINIC-ETAPA
                            :WRK-NULL-03
                     , :WRK-HOST-HFNAL-ETAPA
                            :WRK-NULL-04
                     , :WRK-HOST-RERRO-ETAPA
                            :WRK-NULL-05
                     , :WRK-MSG-TXT
                            :WRK-NULL-07
                     , :WRK-MSG-XML
                            :WRK-NULL-08
                     , :WRK-MSG-A-ENVIAR
                          :WRK-NULL-06)
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS)
           OR (SQLWARN0                EQUAL   'W')
               MOVE 'TMNTRC_MSGRA_SEXTA'
                                       TO ERR-DBD-TAB
               MOVE '3460'             TO ERR-LOCAL
           END-IF.

      *----------------------------------------------------------------*
       3460-99-FIM.                    EXIT.
      *----------------------------------------------------------------*
      *----------------------------------------------------------------*
       3470-INCLUIR-MNTRC-SABADO       SECTION.
      *----------------------------------------------------------------*

           EXEC SQL
              INSERT INTO DB2PRD.TMNTRC_MSGRA_SBADO
                     ( CMNTRC_MSGRA
                     , CPTCAO_TBELA
                     , CSNLZD_MSGRA
                     , CINDCD_ORIGE_TRFGO
                     , CINDCD_ORIGE_PLATF
                     , CIDTFD_PROT_ARQ
                     , HPROCM_MSGEM
                     , HULT_ATULZ
                     , HMOVTO_INFO_MSGEM
                     , HCONF_ENVIO_MSGEM
                     , HCONF_LEITU_MSGEM
                     , CISPB_DSTNA_MSGEM
                     , CISPB_EMISR_MSGEM
                     , CCTRL_RQUIS_INSTC
                     , CUNIC_TRFGO_MSGEM
                     , RIDTFD_MSGEM
                     , NMNTRC_TRFGO_MSGEM
                     , CINDCD_RPROC_MSGEM
                     , CDOMNO_MSGRA_SIST
                     , CSIST_EXTER
                     , CEVNTO_MSGEM
                     , CFLUXO_MSGEM
                     , CFASE_PROCM
                     , CIDTFD_SIT_FASE
                     , HINIC_ETAPA_MNTRC
                     , HFNAL_ETAPA_MNTRC
                     , RERRO_ETAPA_MNTRC
                     , WCONTD_MSGEM_TEXTO
                     , WCONTD_MSGEM_XML
                     , WCONTD_MSGEM_RPROC )
              VALUES ( :WRK-HOST-CMNTRC-MSGRA
                     , :WRK-CPTCAO
                     , :WRK-HOST-CSNLZD-MSGRA
                     , :WRK-HOST-CINDCD-ORIGE
                     , :WRK-HOST-CINDCD-PLATF
                     , NULL
                     , :WRK-HOST-HPROCM-MSGEM
                     , :WRK-HOST-HULT-ATULZ
                     , :WRK-HOST-HMOVTO-INFO
                     , NULL
                     , NULL
                     , :WRK-HOST-CISPB-DSTNA
                     , :WRK-HOST-CISPB-EMISR
                     , :WRK-HOST-CCTRL-INSTC
                     , :WRK-HOST-CUNIC-TRFGO
                     , :WRK-HOST-RIDTFD-MSGEM
                     , NULL
                     , :WRK-HOST-CINDCD-RPROC
                     , :WRK-HOST-CDOMNO-MSGRA
                     , :WRK-HOST-CSIST-EXTER
                     , :WRK-HOST-CEVNTO-MSGEM
                     , :WRK-HOST-CFLUXO-MSGEM
                     , :WRK-HOST-CFASE-PROCM
                            :WRK-NULL-01
                     , :WRK-HOST-SIT-FASE
                            :WRK-NULL-02
                     , :WRK-HOST-HINIC-ETAPA
                            :WRK-NULL-03
                     , :WRK-HOST-HFNAL-ETAPA
                            :WRK-NULL-04
                     , :WRK-HOST-RERRO-ETAPA
                            :WRK-NULL-05
                     , :WRK-MSG-TXT
                            :WRK-NULL-07
                     , :WRK-MSG-XML
                            :WRK-NULL-08
                     , :WRK-MSG-A-ENVIAR
                          :WRK-NULL-06)
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS)
           OR (SQLWARN0                EQUAL   'W')
               MOVE 'TMNTRC_MSGRA_SBADO'
                                       TO ERR-DBD-TAB
               MOVE '3470'             TO ERR-LOCAL
           END-IF.

      *----------------------------------------------------------------*
       3470-99-FIM.                    EXIT.
      *----------------------------------------------------------------*
      *----------------------------------------------------------------*
       3480-INCLUIR-MNTRC-CONTG        SECTION.
      *----------------------------------------------------------------*

           EXEC SQL
              INSERT INTO DB2PRD.TMNTRC_MSGRA_CONTG
                     ( CMNTRC_MSGRA
                     , CPTCAO_TBELA
                     , CSNLZD_MSGRA
                     , CINDCD_ORIGE_TRFGO
                     , CINDCD_ORIGE_PLATF
                     , CIDTFD_PROT_ARQ
                     , HPROCM_MSGEM
                     , HULT_ATULZ
                     , HMOVTO_INFO_MSGEM
                     , HCONF_ENVIO_MSGEM
                     , HCONF_LEITU_MSGEM
                     , CISPB_DSTNA_MSGEM
                     , CISPB_EMISR_MSGEM
                     , CCTRL_RQUIS_INSTC
                     , CUNIC_TRFGO_MSGEM
                     , RIDTFD_MSGEM
                     , NMNTRC_TRFGO_MSGEM
                     , CINDCD_RPROC_MSGEM
                     , CDOMNO_MSGRA_SIST
                     , CSIST_EXTER
                     , CEVNTO_MSGEM
                     , CFLUXO_MSGEM
                     , CFASE_PROCM
                     , CIDTFD_SIT_FASE
                     , HINIC_ETAPA_MNTRC
                     , HFNAL_ETAPA_MNTRC
                     , RERRO_ETAPA_MNTRC
                     , WCONTD_MSGEM_TEXTO
                     , WCONTD_MSGEM_XML
                     , WCONTD_MSGEM_RPROC )
              VALUES ( :WRK-HOST-CMNTRC-MSGRA
                     , :WRK-CPTCAO
                     , :WRK-HOST-CSNLZD-MSGRA
                     , :WRK-HOST-CINDCD-ORIGE
                     , :WRK-HOST-CINDCD-PLATF
                     , NULL
                     , :WRK-HOST-HPROCM-MSGEM
                     , :WRK-HOST-HULT-ATULZ
                     , :WRK-HOST-HMOVTO-INFO
                     , NULL
                     , NULL
                     , :WRK-HOST-CISPB-DSTNA
                     , :WRK-HOST-CISPB-EMISR
                     , :WRK-HOST-CCTRL-INSTC
                     , :WRK-HOST-CUNIC-TRFGO
                     , :WRK-HOST-RIDTFD-MSGEM
                     , NULL
                     , :WRK-HOST-CINDCD-RPROC
                     , :WRK-HOST-CDOMNO-MSGRA
                     , :WRK-HOST-CSIST-EXTER
                     , :WRK-HOST-CEVNTO-MSGEM
                     , :WRK-HOST-CFLUXO-MSGEM
                     , :WRK-HOST-CFASE-PROCM
                            :WRK-NULL-01
                     , :WRK-HOST-SIT-FASE
                            :WRK-NULL-02
                     , :WRK-HOST-HINIC-ETAPA
                            :WRK-NULL-03
                     , :WRK-HOST-HFNAL-ETAPA
                            :WRK-NULL-04
                     , :WRK-HOST-RERRO-ETAPA
                            :WRK-NULL-05
                     , :WRK-MSG-TXT
                            :WRK-NULL-07
                     , :WRK-MSG-XML
                            :WRK-NULL-08
                     , :WRK-MSG-A-ENVIAR
                          :WRK-NULL-06)
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS)
           OR (SQLWARN0                EQUAL   'W')
               MOVE 'TMNTRC_MSGRA_CONTG'
                                       TO ERR-DBD-TAB
               MOVE 'INSERT'           TO ERR-FUN-COMANDO
               MOVE '0380'             TO ERR-LOCAL
               PERFORM 9200-TRATAR-ERRO-DB2
               PERFORM 3300-PROCEDIMENTOS-FINAIS
           END-IF.

      *----------------------------------------------------------------*
       3480-99-FIM.                    EXIT.
      *----------------------------------------------------------------*
      *----------------------------------------------------------------*
       4100-GRAVAR-INCONSISTENTE       SECTION.
      *----------------------------------------------------------------*
      *
           INITIALIZE TMNFB078.

           MOVE WRK-TIMESTAMP          TO HINCL-REG
                                       OF TMNFB078

           MOVE 3                      TO CMSGRA-SIST
                                       OF TMNFB078

           MOVE TMNFWS05-MSG(1:9)      TO CMSGEM-INCOT
                                       OF TMNFB078

           MOVE ZEROS                  TO CIDTFD-TPO-INCON
                                       OF TMNFB078

           MOVE 1                      TO CIDTFD-FLUXO-MSGEM
                                       OF TMNFB078

           MOVE TMNFWE16-FILA-LEGADO   TO IFILA-MSGRA
                                       OF TMNFB078

           MOVE WRK-DESC-WORKF         TO RERRO-ETAPA-MNTRC
                                       OF TMNFB078

           IF TMNFWE05-MENSAGEM        EQUAL SPACES OR LOW-VALUES
              MOVE -1                  TO WRK-WCONTD-MSGEM-NULL
           ELSE
              MOVE ZEROS               TO WRK-WCONTD-MSGEM-NULL

              IF TMNFWE16-TAM-TOTAL    NOT GREATER 32763
                 MOVE TMNFWE16-TAM-TOTAL
                                       TO WCONTD-MSGEM-TRFGO-LEN
                                       OF TMNFB078
              ELSE
                 MOVE 32763            TO WCONTD-MSGEM-TRFGO-LEN
                                       OF TMNFB078
              END-IF

              MOVE TMNFWE05-MENSAGEM   TO WCONTD-MSGEM-TRFGO-TEXT
                                       OF TMNFB078

           END-IF.

           PERFORM 4110-INCOT-NOVA-ESTRT.

           PERFORM 4120-CONTAB-SPBN.

      *----------------------------------------------------------------*
       4100-99-FIM.                    EXIT.
      *----------------------------------------------------------------*
      *----------------------------------------------------------------*
       4110-INCOT-NOVA-ESTRT           SECTION.
      *----------------------------------------------------------------*
           EVALUATE TRUE
              WHEN WRK-DOMINGO
                 PERFORM 4411-INCOT-DOMINGO

              WHEN WRK-SEGUNDA
                 PERFORM 4412-INCOT-SEGUNDA

              WHEN WRK-TERCA
                 PERFORM 4413-INCOT-TERCA

              WHEN WRK-QUARTA
                 PERFORM 4414-INCOT-QUARTA

              WHEN WRK-QUINTA
                 PERFORM 4415-INCOT-QUINTA

              WHEN WRK-SEXTA
                 PERFORM 4416-INCOT-SEXTA

              WHEN WRK-SABADO
                 PERFORM 4417-INCOT-SABADO

              WHEN OTHER
                 PERFORM 4418-INCOT-CONTG
           END-EVALUATE

           IF SQLCODE                  NOT EQUAL ZEROS
              MOVE WRK-PGM-CHAMADOR    TO ERR-PGM
              MOVE WRK-PROGRAMA        TO ERR-MODULO
              MOVE SQLCODE             TO ERR-SQL-CODE
              MOVE 'DB2'               TO ERR-TIPO-ACESSO
              MOVE SPACES              TO ERR-SEGM
              MOVE SQLCA               TO ERR-SQLCA
              MOVE WRK-PROGRAMA        TO ERR-MODULO
              MOVE 'INSERT'            TO ERR-FUN-COMANDO
              PERFORM 9500-GERAR-LOG-ERRO

              PERFORM 4418-INCOT-CONTG
           END-IF.

      *----------------------------------------------------------------*
       4110-99-FIM.                    EXIT.
      *----------------------------------------------------------------*
      *----------------------------------------------------------------*
       4120-CONTAB-SPBN                SECTION.
      *----------------------------------------------------------------*
           INITIALIZE WRK-NP60-AREA.

           SET INCLUI-PROBL            TO TRUE

           MOVE WRK-PROGRAMA           TO SPBN504-PGM-CHAMADOR.

           MOVE '4110'                 TO SPBN504-LOCAL-CHAMADA.


           EXEC CICS
               START
               TRANSID ('NP60')
               FROM (WRK-NP60-AREA)
               LENGTH  (LENGTH OF WRK-NP60-AREA)
               NOHANDLE
           END-EXEC.

           IF (EIBRESP                 NOT EQUAL DFHRESP(NORMAL))
               MOVE 'CIC'              TO ERR-TIPO-ACESSO
               MOVE DFHEIBLK           TO ERR-DFHEIBLK
               MOVE EIBRESP            TO WRK-ERRO-NP60-EIBRESP
               MOVE WRK-ERRO-NP60      TO ERR-TEXTO
               MOVE 1                  TO ERR-RETURN-CODE
               MOVE 'LOC:4120'         TO ERR-TERMINAL
               PERFORM 9500-GERAR-LOG-ERRO
           END-IF.

      *----------------------------------------------------------------*
       4120-99-FIM.                    EXIT.
      *----------------------------------------------------------------*
      *----------------------------------------------------------------*
       4411-INCOT-DOMINGO              SECTION.
      *----------------------------------------------------------------*

           EXEC SQL
               INSERT INTO DB2PRD.TMSGEM_INCOT_DMNGO
                   (HINCL_REG
                   ,CPTCAO_TBELA
                   ,CMSGRA_SIST
                   ,CMSGEM_INCOT
                   ,CIDTFD_TPO_INCON
                   ,CIDTFD_FLUXO_MSGEM
                   ,IFILA_MSGRA
                   ,RERRO_ETAPA_MNTRC
                   ,RCONTD_MSGEM_TRFGO
                   ,WCONTD_MSGEM_TRFGO)
               VALUES
                   (:TMNFB078.HINCL-REG
                   ,:WRK-CPTCAO
                   ,:TMNFB078.CMSGRA-SIST
                           :WRK-CMSGRA-SIST-NULL
                   ,:TMNFB078.CMSGEM-INCOT
                           :WRK-CMSGEM-INCOT-NULL
                   ,:TMNFB078.CIDTFD-TPO-INCON
                           :WRK-CIDTFD-INCON-NULL
                   ,:TMNFB078.CIDTFD-FLUXO-MSGEM
                           :WRK-CIDTFD-FLUXO-NULL
                   ,:TMNFB078.IFILA-MSGRA
                           :WRK-IFILA-MSGRA-NULL
                   ,:TMNFB078.RERRO-ETAPA-MNTRC
                           :WRK-RERRO-ETAPA-NULL
                   ,:TMNFB078.RCONTD-MSGEM-TRFGO
                           :WRK-RCONTD-MSGEM-NULL
                   ,:TMNFB078.WCONTD-MSGEM-TRFGO
                           :WRK-WCONTD-MSGEM-NULL)
           END-EXEC.

           IF SQLCODE                  NOT EQUAL ZEROS
               MOVE 'TMNTRC_MSGRA_DMNGO'
                                       TO ERR-DBD-TAB
               MOVE '4411'             TO ERR-LOCAL
           END-IF.

      *----------------------------------------------------------------*
       4411-99-FIM.                    EXIT.
      *----------------------------------------------------------------*
      *----------------------------------------------------------------*
       4412-INCOT-SEGUNDA              SECTION.
      *----------------------------------------------------------------*

           EXEC SQL
               INSERT INTO DB2PRD.TMSGEM_INCOT_SEGDA
                   (HINCL_REG
                   ,CPTCAO_TBELA
                   ,CMSGRA_SIST
                   ,CMSGEM_INCOT
                   ,CIDTFD_TPO_INCON
                   ,CIDTFD_FLUXO_MSGEM
                   ,IFILA_MSGRA
                   ,RERRO_ETAPA_MNTRC
                   ,RCONTD_MSGEM_TRFGO
                   ,WCONTD_MSGEM_TRFGO)
               VALUES
                   (:TMNFB078.HINCL-REG
                   ,:WRK-CPTCAO
                   ,:TMNFB078.CMSGRA-SIST
                           :WRK-CMSGRA-SIST-NULL
                   ,:TMNFB078.CMSGEM-INCOT
                           :WRK-CMSGEM-INCOT-NULL
                   ,:TMNFB078.CIDTFD-TPO-INCON
                           :WRK-CIDTFD-INCON-NULL
                   ,:TMNFB078.CIDTFD-FLUXO-MSGEM
                           :WRK-CIDTFD-FLUXO-NULL
                   ,:TMNFB078.IFILA-MSGRA
                           :WRK-IFILA-MSGRA-NULL
                   ,:TMNFB078.RERRO-ETAPA-MNTRC
                           :WRK-RERRO-ETAPA-NULL
                   ,:TMNFB078.RCONTD-MSGEM-TRFGO
                           :WRK-RCONTD-MSGEM-NULL
                   ,:TMNFB078.WCONTD-MSGEM-TRFGO
                           :WRK-WCONTD-MSGEM-NULL)
           END-EXEC.

           IF SQLCODE                  NOT EQUAL ZEROS
               MOVE 'TMNTRC_MSGRA_SEGDA'
                                       TO ERR-DBD-TAB
               MOVE '4412'             TO ERR-LOCAL
           END-IF.

      *----------------------------------------------------------------*
       4412-99-FIM.                    EXIT.
      *----------------------------------------------------------------*
      *----------------------------------------------------------------*
       4413-INCOT-TERCA                SECTION.
      *----------------------------------------------------------------*

           EXEC SQL
               INSERT INTO DB2PRD.TMSGEM_INCOT_TRC
                   (HINCL_REG
                   ,CPTCAO_TBELA
                   ,CMSGRA_SIST
                   ,CMSGEM_INCOT
                   ,CIDTFD_TPO_INCON
                   ,CIDTFD_FLUXO_MSGEM
                   ,IFILA_MSGRA
                   ,RERRO_ETAPA_MNTRC
                   ,RCONTD_MSGEM_TRFGO
                   ,WCONTD_MSGEM_TRFGO)
               VALUES
                   (:TMNFB078.HINCL-REG
                   ,:WRK-CPTCAO
                   ,:TMNFB078.CMSGRA-SIST
                           :WRK-CMSGRA-SIST-NULL
                   ,:TMNFB078.CMSGEM-INCOT
                           :WRK-CMSGEM-INCOT-NULL
                   ,:TMNFB078.CIDTFD-TPO-INCON
                           :WRK-CIDTFD-INCON-NULL
                   ,:TMNFB078.CIDTFD-FLUXO-MSGEM
                           :WRK-CIDTFD-FLUXO-NULL
                   ,:TMNFB078.IFILA-MSGRA
                           :WRK-IFILA-MSGRA-NULL
                   ,:TMNFB078.RERRO-ETAPA-MNTRC
                           :WRK-RERRO-ETAPA-NULL
                   ,:TMNFB078.RCONTD-MSGEM-TRFGO
                           :WRK-RCONTD-MSGEM-NULL
                   ,:TMNFB078.WCONTD-MSGEM-TRFGO
                           :WRK-WCONTD-MSGEM-NULL)
           END-EXEC.

           IF SQLCODE                  NOT EQUAL ZEROS
              MOVE 'TMNTRC_MSGRA_TRC'
                                       TO ERR-DBD-TAB
              MOVE '4413'              TO ERR-LOCAL
           END-IF.

      *----------------------------------------------------------------*
       4413-99-FIM.                    EXIT.
      *----------------------------------------------------------------*
      *----------------------------------------------------------------*
       4414-INCOT-QUARTA               SECTION.
      *----------------------------------------------------------------*

           EXEC SQL
               INSERT INTO DB2PRD.TMSGEM_INCOT_QUART
                   (HINCL_REG
                   ,CPTCAO_TBELA
                   ,CMSGRA_SIST
                   ,CMSGEM_INCOT
                   ,CIDTFD_TPO_INCON
                   ,CIDTFD_FLUXO_MSGEM
                   ,IFILA_MSGRA
                   ,RERRO_ETAPA_MNTRC
                   ,RCONTD_MSGEM_TRFGO
                   ,WCONTD_MSGEM_TRFGO)
               VALUES
                   (:TMNFB078.HINCL-REG
                   ,:WRK-CPTCAO
                   ,:TMNFB078.CMSGRA-SIST
                           :WRK-CMSGRA-SIST-NULL
                   ,:TMNFB078.CMSGEM-INCOT
                           :WRK-CMSGEM-INCOT-NULL
                   ,:TMNFB078.CIDTFD-TPO-INCON
                           :WRK-CIDTFD-INCON-NULL
                   ,:TMNFB078.CIDTFD-FLUXO-MSGEM
                           :WRK-CIDTFD-FLUXO-NULL
                   ,:TMNFB078.IFILA-MSGRA
                           :WRK-IFILA-MSGRA-NULL
                   ,:TMNFB078.RERRO-ETAPA-MNTRC
                           :WRK-RERRO-ETAPA-NULL
                   ,:TMNFB078.RCONTD-MSGEM-TRFGO
                           :WRK-RCONTD-MSGEM-NULL
                   ,:TMNFB078.WCONTD-MSGEM-TRFGO
                           :WRK-WCONTD-MSGEM-NULL)
           END-EXEC.

           IF SQLCODE                  NOT EQUAL ZEROS
              MOVE 'TMNTRC_MSGRA_QUART'
                                       TO ERR-DBD-TAB
              MOVE '4414'              TO ERR-LOCAL
           END-IF.

      *----------------------------------------------------------------*
       4414-99-FIM.                    EXIT.
      *----------------------------------------------------------------*
      *----------------------------------------------------------------*
       4415-INCOT-QUINTA               SECTION.
      *----------------------------------------------------------------*

           EXEC SQL
               INSERT INTO DB2PRD.TMSGEM_INCOT_QTA
                   (HINCL_REG
                   ,CPTCAO_TBELA
                   ,CMSGRA_SIST
                   ,CMSGEM_INCOT
                   ,CIDTFD_TPO_INCON
                   ,CIDTFD_FLUXO_MSGEM
                   ,IFILA_MSGRA
                   ,RERRO_ETAPA_MNTRC
                   ,RCONTD_MSGEM_TRFGO
                   ,WCONTD_MSGEM_TRFGO)
               VALUES
                   (:TMNFB078.HINCL-REG
                   ,:WRK-CPTCAO
                   ,:TMNFB078.CMSGRA-SIST
                           :WRK-CMSGRA-SIST-NULL
                   ,:TMNFB078.CMSGEM-INCOT
                           :WRK-CMSGEM-INCOT-NULL
                   ,:TMNFB078.CIDTFD-TPO-INCON
                           :WRK-CIDTFD-INCON-NULL
                   ,:TMNFB078.CIDTFD-FLUXO-MSGEM
                           :WRK-CIDTFD-FLUXO-NULL
                   ,:TMNFB078.IFILA-MSGRA
                           :WRK-IFILA-MSGRA-NULL
                   ,:TMNFB078.RERRO-ETAPA-MNTRC
                           :WRK-RERRO-ETAPA-NULL
                   ,:TMNFB078.RCONTD-MSGEM-TRFGO
                           :WRK-RCONTD-MSGEM-NULL
                   ,:TMNFB078.WCONTD-MSGEM-TRFGO
                           :WRK-WCONTD-MSGEM-NULL)
           END-EXEC.

           IF SQLCODE                  NOT EQUAL ZEROS
              MOVE 'TMNTRC_MSGRA_QTA'  TO ERR-DBD-TAB
              MOVE '4415'              TO ERR-LOCAL
           END-IF.

      *----------------------------------------------------------------*
       4415-99-FIM.                    EXIT.
      *----------------------------------------------------------------*
      *----------------------------------------------------------------*
       4416-INCOT-SEXTA                SECTION.
      *----------------------------------------------------------------*

           EXEC SQL
               INSERT INTO DB2PRD.TMSGEM_INCOT_SEXTA
                   (HINCL_REG
                   ,CPTCAO_TBELA
                   ,CMSGRA_SIST
                   ,CMSGEM_INCOT
                   ,CIDTFD_TPO_INCON
                   ,CIDTFD_FLUXO_MSGEM
                   ,IFILA_MSGRA
                   ,RERRO_ETAPA_MNTRC
                   ,RCONTD_MSGEM_TRFGO
                   ,WCONTD_MSGEM_TRFGO)
               VALUES
                   (:TMNFB078.HINCL-REG
                   ,:WRK-CPTCAO
                   ,:TMNFB078.CMSGRA-SIST
                           :WRK-CMSGRA-SIST-NULL
                   ,:TMNFB078.CMSGEM-INCOT
                           :WRK-CMSGEM-INCOT-NULL
                   ,:TMNFB078.CIDTFD-TPO-INCON
                           :WRK-CIDTFD-INCON-NULL
                   ,:TMNFB078.CIDTFD-FLUXO-MSGEM
                           :WRK-CIDTFD-FLUXO-NULL
                   ,:TMNFB078.IFILA-MSGRA
                           :WRK-IFILA-MSGRA-NULL
                   ,:TMNFB078.RERRO-ETAPA-MNTRC
                           :WRK-RERRO-ETAPA-NULL
                   ,:TMNFB078.RCONTD-MSGEM-TRFGO
                           :WRK-RCONTD-MSGEM-NULL
                   ,:TMNFB078.WCONTD-MSGEM-TRFGO
                           :WRK-WCONTD-MSGEM-NULL)
           END-EXEC.

           IF SQLCODE                  NOT EQUAL ZEROS
              MOVE 'TMNTRC_MSGRA_SEXTA'
                                       TO ERR-DBD-TAB
              MOVE '4416'              TO ERR-LOCAL
           END-IF.

      *----------------------------------------------------------------*
       4416-99-FIM.                    EXIT.
      *----------------------------------------------------------------*
      *----------------------------------------------------------------*
       4417-INCOT-SABADO               SECTION.
      *----------------------------------------------------------------*

           EXEC SQL
               INSERT INTO DB2PRD.TMSGEM_INCOT_SBADO
                   (HINCL_REG
                   ,CPTCAO_TBELA
                   ,CMSGRA_SIST
                   ,CMSGEM_INCOT
                   ,CIDTFD_TPO_INCON
                   ,CIDTFD_FLUXO_MSGEM
                   ,IFILA_MSGRA
                   ,RERRO_ETAPA_MNTRC
                   ,RCONTD_MSGEM_TRFGO
                   ,WCONTD_MSGEM_TRFGO)
               VALUES
                   (:TMNFB078.HINCL-REG
                   ,:WRK-CPTCAO
                   ,:TMNFB078.CMSGRA-SIST
                           :WRK-CMSGRA-SIST-NULL
                   ,:TMNFB078.CMSGEM-INCOT
                           :WRK-CMSGEM-INCOT-NULL
                   ,:TMNFB078.CIDTFD-TPO-INCON
                           :WRK-CIDTFD-INCON-NULL
                   ,:TMNFB078.CIDTFD-FLUXO-MSGEM
                           :WRK-CIDTFD-FLUXO-NULL
                   ,:TMNFB078.IFILA-MSGRA
                           :WRK-IFILA-MSGRA-NULL
                   ,:TMNFB078.RERRO-ETAPA-MNTRC
                           :WRK-RERRO-ETAPA-NULL
                   ,:TMNFB078.RCONTD-MSGEM-TRFGO
                           :WRK-RCONTD-MSGEM-NULL
                   ,:TMNFB078.WCONTD-MSGEM-TRFGO
                           :WRK-WCONTD-MSGEM-NULL)
           END-EXEC.

           IF SQLCODE                  NOT EQUAL ZEROS
              MOVE 'TMNTRC_MSGRA_SBADO'
                                       TO ERR-DBD-TAB
              MOVE '4417'              TO ERR-LOCAL
           END-IF.

      *----------------------------------------------------------------*
       4417-99-FIM.                    EXIT.
      *----------------------------------------------------------------*
      *----------------------------------------------------------------*
       4418-INCOT-CONTG                SECTION.
      *----------------------------------------------------------------*

           EXEC SQL
               INSERT INTO DB2PRD.TMSGEM_INCOT_CONTG
                   (HINCL_REG
                   ,CPTCAO_TBELA
                   ,CMSGRA_SIST
                   ,CMSGEM_INCOT
                   ,CIDTFD_TPO_INCON
                   ,CIDTFD_FLUXO_MSGEM
                   ,IFILA_MSGRA
                   ,RERRO_ETAPA_MNTRC
                   ,RCONTD_MSGEM_TRFGO
                   ,WCONTD_MSGEM_TRFGO)
               VALUES
                   (:TMNFB078.HINCL-REG
                   ,:WRK-CPTCAO
                   ,:TMNFB078.CMSGRA-SIST
                           :WRK-CMSGRA-SIST-NULL
                   ,:TMNFB078.CMSGEM-INCOT
                           :WRK-CMSGEM-INCOT-NULL
                   ,:TMNFB078.CIDTFD-TPO-INCON
                           :WRK-CIDTFD-INCON-NULL
                   ,:TMNFB078.CIDTFD-FLUXO-MSGEM
                           :WRK-CIDTFD-FLUXO-NULL
                   ,:TMNFB078.IFILA-MSGRA
                           :WRK-IFILA-MSGRA-NULL
                   ,:TMNFB078.RERRO-ETAPA-MNTRC
                           :WRK-RERRO-ETAPA-NULL
                   ,:TMNFB078.RCONTD-MSGEM-TRFGO
                           :WRK-RCONTD-MSGEM-NULL
                   ,:TMNFB078.WCONTD-MSGEM-TRFGO
                           :WRK-WCONTD-MSGEM-NULL)
           END-EXEC.

           IF SQLCODE                 NOT EQUAL ZEROS
              MOVE 'TMSGEM_INCOT_CONTG'
                                      TO ERR-DBD-TAB
              MOVE 'INSERT'           TO ERR-FUN-COMANDO
              MOVE '4418'             TO ERR-LOCAL
              PERFORM 9200-TRATAR-ERRO-DB2
              PERFORM 3300-PROCEDIMENTOS-FINAIS
           END-IF.

      *----------------------------------------------------------------*
       4418-99-FIM.                    EXIT.
      *----------------------------------------------------------------*
      *----------------------------------------------------------------*
       5000-INF-BLOCO-RETORNO          SECTION.
      *----------------------------------------------------------------*

           IF TMNFWE16-ENTRADA-MANUAL  NOT EQUAL 'N'
              GO                       TO 5000-99-EXIT
           END-IF.

           MOVE 'RETORNO '             TO TMNFWS03-ID-BLOCO-X
           MOVE WRK-CTRLIF             TO TMNFWS03-CNRO-CTRL-INSTC-X
           MOVE WRK-NUM-UNICO-TMNF     TO TMNFWS03-CNRO-UNIC-SIMM-X
           MOVE WRK-NUOP               TO TMNFWS03-CNRO-UNIC-OPER-X

           MOVE TMNFWE16-CDOM-MSGRA-SPB
                                       TO TMNFWE11-CDOM-MSGRA-SPB
           MOVE WRK-CEMISR-MSGEM       TO TMNFWE11-CISPB-EMISR
           MOVE WRK-MSG                TO TMNFWE11-CEVNTO-MSGEM

           PERFORM 5100-OBTEM-FILA-BLOCO-RET

           IF WRK-MQ-FILA              NOT EQUAL SPACES
              PERFORM 5200-PUT-BLOCO-RETORNO
           END-IF.

      *----------------------------------------------------------------*
       5000-99-EXIT.                   EXIT.
      *----------------------------------------------------------------*
      *----------------------------------------------------------------*
       5100-OBTEM-FILA-BLOCO-RET       SECTION.
      *----------------------------------------------------------------*

           INITIALIZE TMNFB009
                      TMNFB031

           MOVE TMNFWS02-CSIST-EXTER   TO CSIST-EXTER
                                       OF TMNFB031.
           MOVE TMNFWS02-CEVNTO-MSGEM  TO CEVNTO-MSGEM
                                       OF TMNFB031.
           MOVE TMNFWS02-CFLUXO-MSGEM  TO CFLUXO-MSGEM
                                       OF TMNFB031.
           MOVE TMNFWS04-CSIST-PGTO-EMISR
                                       TO CISPB-CONGL
                                       OF TMNFB031.
           MOVE TMNFWE16-CDOM-MSGRA-SPB
                                       TO CDOMNO-MSGRA-SIST
                                       OF TMNFB031.
           MOVE 7                      TO CTPO-FILA-MSGRA
                                       OF TMNFB009
           EXEC SQL
              SELECT VALUE(B.IFILA_MSGRA, ' ')
                INTO :TMNFB009.IFILA-MSGRA
                FROM DB2PRD.TFILA_COMUN_RETOR A
                   , DB2PRD.TFILA_COMUN_SIST B
               WHERE A.CISPB_CONGL     = :TMNFB031.CISPB-CONGL
                 AND A.CSIST_EXTER     = :TMNFB031.CSIST-EXTER
                 AND A.CEVNTO_MSGEM    = :TMNFB031.CEVNTO-MSGEM
                 AND A.CFLUXO_MSGEM    = :TMNFB031.CFLUXO-MSGEM
                 AND A.CDOMNO_MSGRA_SIST
                                       = :TMNFB031.CDOMNO-MSGRA-SIST
                 AND A.NFILA_MSGRA     = B.NFILA_MSGRA
                 AND B.CTPO_FILA_MSGRA = :TMNFB009.CTPO-FILA-MSGRA
               FETCH FIRST ROW ONLY
                WITH UR
           END-EXEC.

           IF (SQLCODE                  NOT EQUAL ZEROS AND +100)
           OR (SQLWARN0                 EQUAL   'W')
               MOVE 'TFILA_COMUN_RETOR'
                                       TO ERR-DBD-TAB
               MOVE 'FETCH'            TO ERR-FUN-COMANDO
               MOVE '0330'             TO ERR-LOCAL
               PERFORM 9200-TRATAR-ERRO-DB2
               PERFORM 3000-FINALIZAR
           END-IF.

           IF SQLCODE                  EQUAL +100
              MOVE SPACES              TO WRK-MQ-FILA
           ELSE
              MOVE IFILA-MSGRA         OF TMNFB009
                                       TO WRK-MQ-FILA
           END-IF.

      *----------------------------------------------------------------*
       5100-99-EXIT.                   EXIT.
      *----------------------------------------------------------------*
      *----------------------------------------------------------------*
       5200-PUT-BLOCO-RETORNO          SECTION.
      *----------------------------------------------------------------*

           MOVE LENGTH                 OF WRK-BLOCO-RETORNO
                                       TO WRK-LLBUFFER-GRAVACAO
           MOVE WRK-BLOCO-RETORNO      TO WRK-MQ-MSGEM-AREA
           MOVE WRK-SBAT3520           TO WRK-MQ-MSGEM-AREA
                                         (WRK-LLBUFFER-GRAVACAO + 1:8)
           ADD 16                      TO WRK-LLBUFFER-GRAVACAO

           MOVE MQ-HANDLE-FILA         TO WRK-HANDLE-FILA.
           MOVE WRK-MQ-FILA            TO WRK-FILA-RETORNO.
           MOVE WRK-HANDLE-FILA        TO WRK-HAND-RETORNO.

           SET WRK-SEM-NOTIF           TO TRUE.
           MOVE SPACES                 TO WRK-FILA-REPLY
                                          WRK-QM-REPLY.
           PERFORM 2825-TRATAR-PUT.

           IF MQ-COMPLETION-CODE       NOT EQUAL ZEROS
              MOVE 'PUT'               TO WRK-ERR-MQ-CMD
              MOVE '0335'              TO WRK-ERR-MQ-LOCAL
              PERFORM 9700-TRATAR-ERRO-MQ
           END-IF.

      *----------------------------------------------------------------*
       5200-99-FIM.                    EXIT.
      *----------------------------------------------------------------*
REPROC*----------------------------------------------------------------*
.      6000-MOVER-RPROC-TXT            SECTION.
.     *----------------------------------------------------------------*
.     * GRAVAR TXT P/ REPROCESSAMENTO EM CASO DE ERRO NA CONVERSAO     *
.     *----------------------------------------------------------------*
.          MOVE TMNFWE16-CDOM-MSGRA-SPB
.                                      TO WRK-REPROC-DOMINIO.
.          MOVE 1                      TO WRK-REPROC-FORMT-MSGEM.
.          MOVE 2                      TO WRK-REPROC-FASE.
.          MOVE WRK-CEMISR-MSGEM       TO WRK-REPROC-ISPB-E.
.          MOVE WRK-CDSTNA-MSGEM       TO WRK-REPROC-ISPB-D.
.          MOVE TMNFWS04-BLOCO-BCMSG   TO WRK-REPROC-BCMSG.
.DAGO*****XMOVE TMNFWS05-BLOCO-SISMSG  TO WRK-REPROC-SISMSG.
.
.          MOVE 'N'                    TO WRK-ACHOU-CHAR.
.          PERFORM                     VARYING IND-2
.                                      FROM
.                                      LENGTH OF WRK-AREA-REPROC-TXT-ENV
.                                      BY -1
.                                      UNTIL IND-2
.                                      LESS 1
.                                      OR WRK-ACHOU-CHAR
.                                      EQUAL 'S'
.             IF  WRK-AREA-REPROC-TXT-ENV(IND-2:1)
.                                      NOT EQUAL SPACES AND
.                 WRK-AREA-REPROC-TXT-ENV(IND-2:1)
.                                      NOT EQUAL LOW-VALUES
.                MOVE 'S'              TO WRK-ACHOU-CHAR
.             END-IF
.          END-PERFORM.
.
.          ADD 1                       TO IND-2.
.          MOVE IND-2                  TO WRK-TAMANHO-REAL.
.
.          MOVE 'N'                    TO WRK-ACHOU-CHAR.
.          PERFORM                     VARYING IND-3
.                                      FROM
.                                      LENGTH OF TMNFWS05-BLOCO-SISMSG
.                                      BY -1
.                                      UNTIL IND-3
.                                      LESS 1
.                                      OR WRK-ACHOU-CHAR
.                                      EQUAL 'S'
.             IF  TMNFWS05-BLOCO-SISMSG(IND-3:1)
.                                      NOT EQUAL SPACES AND
.                 TMNFWS05-BLOCO-SISMSG(IND-3:1)
.                                      NOT EQUAL LOW-VALUES
.                MOVE 'S'              TO WRK-ACHOU-CHAR
.             END-IF
.          END-PERFORM.
.
.          ADD 1                       TO IND-3.
.          ADD IND-3                   TO WRK-TAMANHO-REAL.
.
.          MOVE WRK-TAMANHO-REAL       TO WRK-MSG-A-ENVIAR-LENGTH.
.          MOVE WRK-AREA-REPROC-TXT-ENV(1:WRK-TAMANHO-REAL)
.                                      TO WRK-MSG-A-ENVIAR-DATA
.                                        (1:LENGTH OF
.                                           WRK-AREA-REPROC-TXT-ENV).
.          MOVE TMNFWS05-BLOCO-SISMSG(1:IND-3)
.                                      TO WRK-MSG-A-ENVIAR-DATA
.                                        (LENGTH OF
.                                         WRK-AREA-REPROC-TXT-ENV + 1
.                                         :IND-3).
.
.     *----------------------------------------------------------------*
.      6000-99-EXIT.                   EXIT.
.     *----------------------------------------------------------------*
.     *----------------------------------------------------------------*
.      6100-MOVER-RPROC-XML           SECTION.
.     *----------------------------------------------------------------*
.     * GRAVAR XML P/ REPROCESSAMENTO EM CASO DE ERRO NA CRIPTOGRAFIA  *
.     *----------------------------------------------------------------*
.          MOVE TMNFW820-S-TAM-XML     TO WRK-TAMANHO-REAL.
.
.          MOVE WRK-TAMANHO-REAL       TO WRK-MSG-A-ENVIAR-LENGTH.
.          MOVE TMNFW820-S-XML(1:WRK-TAMANHO-REAL)
.                                      TO WRK-MSG-A-ENVIAR-DATA.
.
.     *----------------------------------------------------------------*
.      6100-99-EXIT.                   EXIT.
.     *----------------------------------------------------------------*
.     *----------------------------------------------------------------*
.      6200-MOVER-RPROC-CRIPTO         SECTION.
.     *----------------------------------------------------------------*
.     * GRAVAR XML CRIPTOGRAFADO P/ REPROCESSAMENTO EM CASO DE ERRO NO *
.     * ENVIO PARA BACEN/CAMARA                                        *
.     *----------------------------------------------------------------*
.
181218     IF WRK-SCRY-CONV-DOM        EQUAL '0'
.             MOVE LNK-301-LEN-AREA    TO WRK-TAMANHO-REAL
.
.             MOVE WRK-TAMANHO-REAL    TO WRK-MSG-A-ENVIAR-LENGTH
.             MOVE LNK-301-AREA(1:WRK-TAMANHO-REAL)
.                                      TO WRK-MSG-A-ENVIAR-DATA
181218     ELSE
181218        MOVE LNK-331-LEN-AREA    TO WRK-TAMANHO-REAL
181218
181218        MOVE WRK-TAMANHO-REAL    TO WRK-MSG-A-ENVIAR-LENGTH
181218        MOVE LNK-331-AREA(1:WRK-TAMANHO-REAL)
181218                                 TO WRK-MSG-A-ENVIAR-DATA
181218     END-IF.
.
.     *----------------------------------------------------------------*
.      6200-99-EXIT.                   EXIT.
REPROC*----------------------------------------------------------------*
030217*----------------------------------------------------------------*
.     * GRAVAR DADOS NA TABELA TEMPORARIA                              *
.     *----------------------------------------------------------------*
.      7000-GRAVAR-TAB-TEMP            SECTION.
.     *----------------------------------------------------------------*
.
.          IF (WRK-NUM-UNICO-TMNF       EQUAL SPACES OR LOW-VALUES)
.              GO TO 7000-99-FIM
.          END-IF.
.
.          PERFORM 7100-VALORIZAR-BASE-TEMP.
.
.          PERFORM 7200-VERIFICAR-CONTINUACAO
.
.          PERFORM 7300-START-SPBN5001.
.
.     *----------------------------------------------------------------*
.      7000-99-FIM.                    EXIT.
.     *----------------------------------------------------------------*
.     *----------------------------------------------------------------*
.     * VALORIZAR DADOS PARA GRAVACAO NA TABELA TEMPORARIA             *
.     *----------------------------------------------------------------*
.      7100-VALORIZAR-BASE-TEMP        SECTION.
.     *----------------------------------------------------------------*
.          INITIALIZE SPBNWNP5-DADOS-BASICO
.
.          MOVE WRK-NUM-UNICO-TMNF     TO SPBNWNP5-CMNTRC-MSGRA
.          MOVE WRK-TIMESTAMP          TO SPBNWNP5-HPROCM-MSGEM
.          MOVE WRK-CDSTNA-MSGEM       TO SPBNWNP5-CISPB-DSTNA-MSGEM
.          MOVE WRK-CEMISR-MSGEM       TO SPBNWNP5-CISPB-EMISR-MSGEM
.          MOVE WRK-CTRLIF             TO SPBNWNP5-CCTRL-RQUIS-INSTC
.          MOVE WRK-NUOP               TO SPBNWNP5-CUNIC-TRFGO-MSGEM
.          MOVE TMNFWE04-MQID-48       TO SPBNWNP5-RIDTFD-MSGEM
.          MOVE WRK-MSG                TO SPBNWNP5-RMNTRC-TRFGO-MSGEM
.          MOVE TMNFWE16-USUARIO       TO SPBNWNP5-CUSUAR-MANUT
.
181218     IF WRK-SCRY-CONV-DOM        EQUAL '0'
030217        MOVE LNK-301-HEA         TO SPBNWNP5-RHEAD-SEGRC-CMARA
181218     ELSE
181218        MOVE LNK-331-HEA         TO SPBNWNP5-RHEAD-SEGRC-CMARA
181218     END-IF.
.
.          MOVE WRK-CSNLZD-MSGRA       TO SPBNWNP5-CSNLZD-MSGRA
.          MOVE ZEROS                  TO SPBNWNP5-CSNLZD-MSGRA-N
.
.          IF  TMNFWE16-ENTRADA-MANUAL EQUAL 'S'
.              MOVE WRK-ENVIO-MANUAL   TO SPBNWNP5-CINDCD-ORIGE-TRFGO
.          ELSE
.              MOVE WRK-ENVIO-LEGADO   TO SPBNWNP5-CINDCD-ORIGE-TRFGO
.          END-IF.
.          MOVE ZEROS              TO SPBNWNP5-CINDCD-ORIGE-TRFGO-N
.
.          MOVE WRK-MF             TO SPBNWNP5-CINDCD-ORIGE-PLATF
.          MOVE ZEROS              TO SPBNWNP5-CINDCD-ORIGE-PLATF-N
.
.          MOVE ZEROS              TO SPBNWNP5-CIDTFD-PROT-ARQ
.          MOVE -1                 TO SPBNWNP5-CIDTFD-PROT-ARQ-N
.
.
.          MOVE SPACES             TO SPBNWNP5-HCONF-ENVIO-MSGEM
.                                     SPBNWNP5-HCONF-LEITU-MSGEM
.          MOVE -1                 TO SPBNWNP5-HCONF-ENVIO-MSGEM-N
.                                     SPBNWNP5-HCONF-LEITU-MSGEM-N
.
.          MOVE TMNFWE16-HOCORR-WKFLW
.                                  TO SPBNWNP5-HMOVTO-INFO-MSGEM
.          MOVE ZEROS              TO SPBNWNP5-HMOVTO-INFO-MSGEM-N
.
.          MOVE ZEROS              TO SPBNWNP5-NMNTRC-TRFGO-MSGEM
.          MOVE -1                 TO SPBNWNP5-NMNTRC-TRFGO-MSGEM-N
.
.          MOVE WRK-CINDCD-RPROC-AUX
.                                  TO SPBNWNP5-CINDCD-RPROC-MSGEM
.
.          MOVE TMNFWE16-CDOM-MSGRA-SPB
.                                  TO SPBNWNP5-CDOMNO-MSGRA-SIST
.          MOVE -1                 TO SPBNWNP5-CDOMNO-MSGRA-SIST-N
.
.          MOVE WRK-PROGRAMA       TO SPBNWNP5-PGM-ORIGEM
.
.          IF WRK-MSG-SIST         EQUAL 'DDA'
.             MOVE WRK-CFASE-PROCM(WRK-TMNFB029-NREG)
.                                  TO SPBNWNP5-CFASE-PROCM
.             MOVE WRK-SIT-FASE-B29(WRK-TMNFB029-NREG)
.                                  TO SPBNWNP5-CIDTFD-SIT-FASE
.             MOVE WRK-HINIC-MNTRC(1)
.                                  TO SPBNWNP5-HINIC-ETAPA-MNTRC
.             MOVE WRK-RERRO-MNTRC(WRK-TMNFB029-NREG)
.                                  TO SPBNWNP5-RERRO-ETAPA-MNTRC
.
.             MOVE ZEROS           TO SPBNWNP5-CFASE-PROCM-N
.                                     SPBNWNP5-CIDTFD-SIT-FASE-N
.                                     SPBNWNP5-HINIC-ETAPA-MNTRC-N
.                                     SPBNWNP5-RERRO-ETAPA-MNTRC-N
.
.          ELSE
.             MOVE ZEROS               TO SPBNWNP5-CFASE-PROCM
.                                         SPBNWNP5-CIDTFD-SIT-FASE
.
.             MOVE SPACES              TO SPBNWNP5-HINIC-ETAPA-MNTRC
.                                         SPBNWNP5-RERRO-ETAPA-MNTRC
.
.             MOVE -1                  TO SPBNWNP5-CFASE-PROCM-N
.                                         SPBNWNP5-CIDTFD-SIT-FASE-N
.                                         SPBNWNP5-HINIC-ETAPA-MNTRC-N
.                                         SPBNWNP5-RERRO-ETAPA-MNTRC-N
.          END-IF.
.
.          INITIALIZE SPBNWNP5-BLOCO-MSG
.          MOVE WRK-MSG-XML-LENGTH     TO SPBNWNP5-BLOCO-MSG-LEN
.
.          MOVE WRK-MSG-XML-DATA(1:WRK-MSG-XML-LENGTH)
.                                      TO SPBNWNP5-BLOCO-MSG-TEXT.
.
.          COMPUTE WRK-TAM-DADOS-BASE-TEMP
.                                      = LENGTH
.                                      OF SPBNWNP5-DADOS-BASICO
.                                      + LENGTH
.                                      OF SPBNWNP5-BLOCO-MSG-LEN
.                                      +  SPBNWNP5-BLOCO-MSG-LEN.
.
.
.     *----------------------------------------------------------------*
.      7100-99-FIM.                    EXIT.
.     *----------------------------------------------------------------*
.     *----------------------------------------------------------------*
.      7200-VERIFICAR-CONTINUACAO      SECTION.
.     *----------------------------------------------------------------*
.
.          SET WRK-ERRO-SPBN-N         TO TRUE.
.
.          IF (WRK-TAM-DADOS-BASE-TEMP GREATER 32000)
.              SET SPBNWNP5-CONTINUA-S TO TRUE
.              MOVE EIBTRMID           TO WRK-TERM-TS
.                                         SPBNWNP5-TERMID
.              COMPUTE WRK-TAM-DADOS-TS
.                                      =  WRK-TAM-DADOS-BASE-TEMP
.                                      -  32000
.              MOVE SPBNWNP5-DADOS-SPBN
.                                 (32001:WRK-TAM-DADOS-TS)
.                                      TO WRK-DADOS-TS
.              PERFORM 7210-DELETAR-TS
.              PERFORM 7220-GRAVAR-TS
.          ELSE
.              SET SPBNWNP5-CONTINUA-N TO TRUE
.          END-IF.
.
.
.     *----------------------------------------------------------------*
.      7200-99-FIM.                    EXIT.
.     *----------------------------------------------------------------*
.     *----------------------------------------------------------------*
.      7210-DELETAR-TS                 SECTION.
.     *----------------------------------------------------------------*
.
.          EXEC CICS
.              DELETEQ TS
.              QNAME  (WRK-NOME-TS)
.              NOHANDLE
.          END-EXEC.
.
.          IF (EIBRESP                 NOT EQUAL ZEROS AND 44)
.              SET WRK-ERRO-SPBN-S     TO TRUE
.              MOVE 'CIC'              TO ERR-TIPO-ACESSO
.              MOVE DFHEIBLK           TO ERR-DFHEIBLK
.              MOVE EIBRESP            TO WRK-ERRO-NP50-RESP-1
.              MOVE WRK-ERRO-NP50      TO ERR-TEXTO
.              MOVE 1                  TO ERR-RETURN-CODE
.              MOVE 'LOC:7210'         TO ERR-TERMINAL
.              PERFORM 9500-GERAR-LOG-ERRO
.          END-IF.
.
.     *----------------------------------------------------------------*
.      7210-99-FIM.                    EXIT.
.     *----------------------------------------------------------------*
.     *----------------------------------------------------------------*
.      7220-GRAVAR-TS                  SECTION.
.     *----------------------------------------------------------------*
.          EXEC CICS
.               WRITEQ TS
.               QNAME (WRK-NOME-TS)
.               FROM  (WRK-DADOS-TS)
.               LENGTH(LENGTH OF WRK-DADOS-TS)
.               NOHANDLE
.          END-EXEC.
.
.          IF (EIBRESP                 NOT EQUAL ZEROS)
.              SET WRK-ERRO-SPBN-S     TO TRUE
.              MOVE 'CIC'              TO ERR-TIPO-ACESSO
.              MOVE DFHEIBLK           TO ERR-DFHEIBLK
.              MOVE EIBRESP            TO WRK-ERRO-NP50-RESP-2
.              MOVE WRK-ERRO-NP50      TO ERR-TEXTO
.              MOVE 1                  TO ERR-RETURN-CODE
.              MOVE 'LOC:7220'         TO ERR-TERMINAL
.              PERFORM 9500-GERAR-LOG-ERRO
.          END-IF.
.
.     *----------------------------------------------------------------*
.      7220-99-FIM.                    EXIT.
.     *----------------------------------------------------------------*
.     *----------------------------------------------------------------*
.      7300-START-SPBN5001             SECTION.
.     *----------------------------------------------------------------*
.
.          IF (WRK-ERRO-SPBN-S)
.              GO TO 7300-99-FIM
.          END-IF.
.
.          MOVE SPBNWNP5-DADOS-SPBN(1:32000)
.                                      TO WRK-AREA-SPBN5001.
.
.          EXEC CICS
.              START
.              TRANSID ('NP50')
.              FROM    (WRK-AREA-SPBN5001)
.              LENGTH  (LENGTH OF WRK-AREA-SPBN5001)
.              NOHANDLE
.          END-EXEC.
.
.          IF (EIBRESP                 NOT EQUAL DFHRESP(NORMAL))
.              MOVE 'CIC'              TO ERR-TIPO-ACESSO
.              MOVE DFHEIBLK           TO ERR-DFHEIBLK
.              MOVE EIBRESP            TO WRK-ERRO-NP50-EIBRESP
.              MOVE WRK-ERRO-NP50      TO ERR-TEXTO
.              MOVE 1                  TO ERR-RETURN-CODE
.              MOVE 'LOC:0320'         TO ERR-TERMINAL
.              PERFORM 9500-GERAR-LOG-ERRO
.          END-IF.
.
.     *----------------------------------------------------------------*
.      7300-99-FIM.                    EXIT.
030217*----------------------------------------------------------------*
      *----------------------------------------------------------------*
      * SAIDA COM ERRO "VOLTA FILA"                                    *
      *----------------------------------------------------------------*
       9100-ERRO                       SECTION.
      *----------------------------------------------------------------*

           IF TMNFWE16-ENTRADA-MANUAL  EQUAL 'S'
              IF ERR-TIPO-ACESSO       EQUAL 'DB2'
                 MOVE SPACES           TO  WRK-ERRO-GERAL
                 MOVE ERR-MODULO       TO  WRK-ERR-EM-PGM
                 MOVE ERR-LOCAL        TO  WRK-ERR-EM-LOCAL
                 MOVE ERR-DBD-TAB      TO  WRK-ERR-TABELA
                 MOVE ERR-FUN-COMANDO
                                       TO  WRK-ERR-COMANDO
                 MOVE ERR-SQL-CODE     TO  WRK-SQL-CODE
                 MOVE WRK-SQL-CODE-3   TO  WRK-ERR-SQLCOD
                 MOVE WRK-MSG-ENTRADA-MANUAL
                                       TO  ERR-TEXTO-MSG
              END-IF
           END-IF.

      *----------------------------------------------------------------*
       9100-99-EXIT.                   EXIT.
      *----------------------------------------------------------------*
      *----------------------------------------------------------------*
       9200-TRATAR-ERRO-DB2            SECTION.
      *----------------------------------------------------------------*

           MOVE WRK-PGM-CHAMADOR       TO ERR-PGM
           MOVE WRK-PROGRAMA           TO ERR-MODULO
           MOVE SQLCODE                TO ERR-SQL-CODE.
           MOVE 'DB2'                  TO ERR-TIPO-ACESSO.
           MOVE SPACES                 TO ERR-SEGM.
           MOVE SQLCA                  TO ERR-SQLCA.
           MOVE WRK-PROGRAMA           TO ERR-MODULO.

           IF SQLCODE                  EQUAL -911
              MOVE 1                   TO ERR-RETURN-CODE
           ELSE
              MOVE 4                   TO ERR-RETURN-CODE
           END-IF

           PERFORM 9400-ROLLBACK.
           PERFORM 9500-GERAR-LOG-ERRO.
           PERFORM 9100-ERRO.

           MOVE ERR-LOCAL           TO WRK-ERR-DB2-LOCAL
           MOVE SQLCODE             TO WRK-ERR-DB2-SQLCODE
           MOVE SQLCA(1:LENGTH      OF WRK-ERR-DB2-SQLCA)
                                    TO WRK-ERR-DB2-SQLCA
           MOVE WRK-ERR-DB2         TO WRK-MSG-ERR-APL
           PERFORM 9600-TRATAR-ERRO-APL.

      *----------------------------------------------------------------*
       9200-99-FIM.                    EXIT.
      *----------------------------------------------------------------*
      *----------------------------------------------------------------*
       9300-TRATAR-ERRO-CICS           SECTION.
      *----------------------------------------------------------------*

           MOVE WRK-PROGRAMA           TO ERR-PGM
           MOVE DFHEIBLK               TO ERR-DFHEIBLK
           MOVE 'CIC'                  TO ERR-TIPO-ACESSO.
           MOVE SPACES                 TO ERR-SEGM
                                          ERR-FUN-COMANDO
                                          ERR-DBD-TAB.
           MOVE SQLCA                  TO ERR-SQLCA.
           MOVE 3                      TO ERR-RETURN-CODE.

           PERFORM 9400-ROLLBACK.
           PERFORM 9500-GERAR-LOG-ERRO.
           PERFORM 9100-ERRO.


           MOVE ERR-LOCAL              TO WRK-ERR-CICS-LOCAL
           MOVE EIBRESP                TO WRK-ERR-CICS-EIBRESP
           MOVE EIBRESP2               TO WRK-ERR-CICS-EIBRESP2
           MOVE WRK-ERR-CICS           TO WRK-MSG-ERR-APL

           PERFORM 9600-TRATAR-ERRO-APL.

      *----------------------------------------------------------------*
       9300-99-FIM.                    EXIT.
      *----------------------------------------------------------------*
      *----------------------------------------------------------------*
       9400-ROLLBACK                   SECTION.
      *----------------------------------------------------------------*
      *
           EXEC CICS SYNCPOINT
               ROLLBACK
           END-EXEC.
      *
           IF (EIBRESP                 NOT EQUAL DFHRESP(NORMAL))
                CONTINUE
           END-IF
           .
      *----------------------------------------------------------------*
       9400-99-FIM.                    EXIT.
      *----------------------------------------------------------------*
      *----------------------------------------------------------------*
       9500-GERAR-LOG-ERRO             SECTION.
      *----------------------------------------------------------------*
           MOVE 'VRS001'               TO ERR-VERSAO
           MOVE 'LOC:'                 TO ERR-TERMINAL
           MOVE ERR-LOCAL              TO ERR-TERMINAL(5:)

           EXEC  CICS ASSIGN
                 APPLID  (ERR-CICS)
           END-EXEC.

           EXEC  CICS START
               TRANSID     ('CD11')
               FROM        (ERRO-AREA)
               LENGTH      (LENGTH OF ERRO-AREA)
               NOHANDLE
           END-EXEC.

           IF (EIBRESP                 NOT EQUAL DFHRESP(NORMAL))
               CONTINUE
           END-IF.

      *----------------------------------------------------------------*
       9500-99-FIM.                    EXIT.
      *----------------------------------------------------------------*
      *----------------------------------------------------------------*
       9600-TRATAR-ERRO-APL            SECTION.
      *----------------------------------------------------------------*

           MOVE WRK-PGM-CHAMADOR       TO ERR-PGM
           MOVE WRK-PROGRAMA           TO ERR-MODULO
           MOVE 'APL'                  TO ERR-TIPO-ACESSO
           MOVE 4                      TO ERR-RETURN-CODE.

           MOVE WRK-MSG-ERR-APL        TO ERR-TEXTO-MSG.

           PERFORM 9500-GERAR-LOG-ERRO.

      *----------------------------------------------------------------*
       9600-99-FIM.                    EXIT.
      *----------------------------------------------------------------*
      *----------------------------------------------------------------*
       9700-TRATAR-ERRO-MQ             SECTION.
      *----------------------------------------------------------------*

           MOVE WRK-MQ-FILA(1:40)      TO WRK-ERR-MQ-FILA
           MOVE MQ-REASON-CODE         TO WRK-LEN-AREA-9
           MOVE WRK-LEN-AREA-X         TO WRK-ERR-MQ-REASON

           MOVE WRK-PGM-CHAMADOR       TO ERR-PGM
           MOVE WRK-PROGRAMA           TO ERR-MODULO
           MOVE 'APL'                  TO ERR-TIPO-ACESSO.
           MOVE WRK-ERR-MQ             TO ERR-TEXTO-MSG
           MOVE WRK-PROGRAMA           TO ERR-MODULO
           MOVE 3                      TO ERR-RETURN-CODE.

           PERFORM 9500-GERAR-LOG-ERRO.

      *----------------------------------------------------------------*
       9700-99-FIM.                    EXIT.
      *----------------------------------------------------------------*
      *----------------------------------------------------------------*
       9800-TRATAR-ABEND               SECTION.
      *----------------------------------------------------------------*

           MOVE WRK-PGM-CHAMADOR       TO ERR-PGM
           MOVE WRK-PROGRAMA           TO ERR-MODULO
           MOVE 'APL'                  TO ERR-TIPO-ACESSO
           MOVE 4                      TO ERR-RETURN-CODE.

           EXEC  CICS ASSIGN ABCODE(WRK-ABCODE)
           END-EXEC.

           MOVE WRK-ERR-ABEND          TO ERR-TEXTO-MSG

           PERFORM 9500-GERAR-LOG-ERRO.
           PERFORM 9400-ROLLBACK.
           PERFORM 3300-PROCEDIMENTOS-FINAIS.

      *----------------------------------------------------------------*
       9800-99-FIM.                    EXIT.
      *----------------------------------------------------------------*
