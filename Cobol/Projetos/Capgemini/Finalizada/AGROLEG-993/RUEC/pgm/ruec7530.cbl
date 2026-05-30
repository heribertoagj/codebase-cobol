      *===============================================================*
       IDENTIFICATION DIVISION.
      *===============================================================*


       PROGRAM-ID. RUEC7530.
      *===============================================================*
      *                   C P M   S I S T E M A S                     *
      *---------------------------------------------------------------*
      *                                                               *
      *      PROGRAMADOR  : DORIVAL            -   CPM                *
      *      SUPERVISOR   : CARLOS             -   CPM                *
      *      ANALISTA     : CARLOS BOOG        -   GP82               *
      *      DATA         : 23/06/1998                                *
      *                                                               *
      *      OBJETIVO     :                                           *
      *        LISTAR IMOVEIS DO PROPONENTE                           *
      *                                                               *
      *      BANCO DE DADOS:                                          *
      *         DB2                                                   *
      *           TABLE                          INCLUDE/BOOK         *
      *           DB2PRD.TIMOV_RURAL_EXPDO         RUECB056           *
      *           DB2PRD.TIMOV_RURAL_ARREN         RUECB055           *
      *           DB2PRD.TRBNHO_IMOV_RURAL         RUECB060           *
      *           DB2PRD.TRSUMO_CLI_RURAL          RUECB063           *
      *           DB2PRD.TCULTR_AGRIC_IMOV         RUECB051           *
      *           DB2PRD.NAVEGACAO_RURAL           RUECB042           *
      *           DB2PRD.V01FINRURA_PRORURA        RURCV010           *
      *                                                               *
      *      TELAS                                                    *
      *      -----                                                    *
      *      RUECT753                                                 *
      *                                                               *
      *      MODULOS CHAMADOS                                         *
      *      ----------------                                         *
      *      - POOL0660                                               *
      *      - POOL5000                                               *
      *      - POOL7100                                               *
      *      - POOL7600                                               *
      *      - CADU8430                                               *
      *                                                               *
      *===============================================================*
POL097*                                                               *
POL097*                       A  L  T  E  R  A  C  A  O               *
POL097*                       -------------------------               *
POL097*                                                               *
POL097*   ANALISTA      : MARINA FUJITA - POLEN TECHNOLOGIES          *
POL097*   DATA          : JULHO/2005                                  *
POL097*   PROJETO       : POL 0097/04 - RUEC X CADU                   *
POL097*   OBJETIVO      : SUBSTITUIR TABELAS E PROCEDIMENTOS FICA     *
POL097*                   POR CADU E CADASTRO RURAL RUEC              *
POL097*                                                               *
BSI083*================================================================*
BSI083*                B S I   T E C N O L O G I A                     *
BSI083*================================================================*
BSI083*    ANALISTA....:   ODIRLEI COSTA           - BSI TECNOLOGIA    *
BSI083*    DATA........:   AGOSTO/2014                                 *
BSI083*    PROJETO.....:   PWT 14/0083                                 *
BSI083*                    ALIENACAO FIDUCIARIA                        *
BSI083*    OBJETIVO....:   ADEQUAR PARA NAO TRATAR MAIS HIPOTECAS E    *
BSI083*                    SIM COMO IMOVEIS DADOS COMO GARANTIA        *
BSI083*================================================================*
CAR   *===============================================================*
CAR   *                                                               *
CAR   *        ANALISTA : EVANDRO GUIMARAES - BRQ IT                  *
CAR   *        DATA     : MAIO / 2018                                 *
CAR   *        OBJETIVO : TRATAR TIPO DE BENEFICIARIO ANTES DO IMOVEL *
CAR   *                                                               *
CAR   *===============================================================*
PRI499*                ALTERACAO - PRIME                               *
PRI499*----------------------------------------------------------------*
PRI499*                                                                *
PRI499* ANALISTA REPONSAVEL: WALTER MESSAS                             *
PRI499* DATA...............: 24/11/2022                                *
PRI499* OBJETIVOS..........: INCLUSAO DE TIPO DE CLIMA NO CODIGO       *
PRI499*                      DO SISTEMA DE PRODUCAO                    *
PRI499*================================================================*
STEFAB*                ALTERACAO - STEFANINI                           *
.     *----------------------------------------------------------------*
.     * ANALISTA REPONSAVEL: FABRICA - STEFANINI                       *
.     * DATA...............: 20/10/2024                                *
.     * OBJETIVOS..........: VERIFICAR CONSISTENCIA DOS CODIGOS        *
.     *                      NIRF E SNCR (CCIR)                        *
STEFAB*================================================================*
220725*                ALTERACAO - 7COMM                               *
.     *----------------------------------------------------------------*
.     * ANALISTA REPONSAVEL: FABRICA - 7COMM                           *
.     * DATA...............: 22/07/2025                                *
.     * OBJETIVOS..........: TRATAR CONSISTENCIA DOS CODIGOS NIRF E    *
.     *                      CCIR SEPARADAMENTE PARA APLICAR NOVAS     *
.     *                      REGRAS NO TIPO DE CONFORMIDADE            *
220725*================================================================*
180825*                ALTERACAO - 7COMM                               *
.     *----------------------------------------------------------------*
.     * ANALISTA REPONSAVEL: FABRICA - 7COMM                           *
.     * DATA...............: 18/08/2025                                *
.     * OBJETIVOS..........: ADEQUAR GRAVACAO DOS CAMPOS IND-TEM-NIRF  *
.     *                      E IND-TEM-CCIR                            *
180825*                      INCLUSAO DO CAMPO IND-TEM-IMOVEL          *
CCM01A*================================================================*
.     * ANALISTA REPONSAVEL: CRISTIANY MACAN                           *
.     * DATA...............: JANEIRO - 2026                            *
CCM01A* OBJETIVOS..........: AGROLEG-404                               *
      *================================================================*

           EJECT
      *===============================================================*
       ENVIRONMENT DIVISION.
      *===============================================================*

      *---------------------------------------------------------------*
       CONFIGURATION SECTION.
      *---------------------------------------------------------------*

       SPECIAL-NAMES.
           DECIMAL-POINT IS COMMA.

           EJECT
      *===============================================================*
       DATA DIVISION.
      *===============================================================*

      *---------------------------------------------------------------*
       WORKING-STORAGE SECTION.
      *---------------------------------------------------------------*

POL097 01  WRK-RUECB056-NULL.
POL097     03 WRK-SEQ-PATRM-NULL            PIC S9(04) COMP VALUE +0.

CAR   *---------------------------------------------------------------*
CAR    01  FILLER                      PIC X(32)           VALUE
CAR        '*** AUXILIARES TRAT. CAR     ***'.
CAR   *---------------------------------------------------------------*
CAR
CAR        COPY 'I#RUEC58'.
CAR
CAR    01  FILLER.
CAR        10  WRK-RUEC8800            PIC X(08) VALUE 'RUEC8800'.
CAR        10  WRK-DATA-CEDULA         PIC X(10) VALUE SPACES.
CAR
BR0518 01  WRK-ALPFA-1.
BR0518     05  WRK-NUMERICO-1          PIC 9(01) VALUE ZEROS.

      *---------------------------------------------------------------*
      ***                DEFINICAO DE AREAS E FUNCOES
      *---------------------------------------------------------------*

       77  WRK-GU                      PIC X(04)         VALUE 'GU'.
       77  WRK-CHNG                    PIC X(04)         VALUE 'CHNG'.
       77  WRK-ISRT                    PIC X(04)         VALUE 'ISRT'.

       77  WRK-COD-USER                PIC 9(07)         VALUE ZEROS.
       77  WRK-FUNCAO                  PIC X(04)         VALUE SPACES.
       77  WRK-NOVA-OPCAO              PIC 9(02)         VALUE ZEROS.
       77  WRK-OPCAO-ATUAL             PIC 9(02)         VALUE ZEROS.
       77  WRK-TELA                    PIC X(08)         VALUE SPACES.
       77  WRK-ERRO                    PIC X(01)         VALUE SPACE.
POL097 77  WRK-RUEC7722                PIC X(08)     VALUE 'RUEC7722'.
POL097 77  WRK-CADU8430                PIC X(08)     VALUE 'CADU8430'.

       77  IND                         PIC 9(03) COMP-3  VALUE ZEROS.
       77  IND-1                       PIC 9(03) COMP-3  VALUE ZEROS.
       77  WRK-QT-DESPREZAR            PIC 9(15) COMP-3  VALUE ZEROS.
       77  WRK-PGM-CHAMADOR            PIC X(08)         VALUE SPACES.
       77  WRK-PASSAR-CONTROLE         PIC 9(01)         VALUE ZEROS.
       77  WRK-TEM-REGISTROS           PIC 9(01)         VALUE ZEROS.
       77  WRK-ACHEI-CSEQ-IMOV         PIC 9(01)         VALUE ZEROS.
       77  WRK-MSG                     PIC 9(02)         VALUE ZEROS.
       77  WRK-OCCUR                   PIC 9(02)         VALUE ZEROS.
       77  WRK-QTSEL                   PIC 9(02)         VALUE ZEROS.
       77  WRK-QTDE-BRANCO             PIC 9(02)         VALUE ZEROS.
       77  WRK-QTPRINC                 PIC 9(01)         VALUE ZEROS.
       77  WRK-QTDE-IMOV-PRINC         PIC 9(02)         VALUE ZEROS.
       77  WRK-MSG-POOL1660            PIC X(79)         VALUE SPACES.
       77  WRK-NOME-POOL1660           PIC X(40)         VALUE SPACES.
       77  WRK-NAO-ACHOU               PIC X             VALUE 'N'.

       77  IND-SORT                    PIC S9(03) COMP-3  VALUE ZEROS.
       77  IND-SORT2                   PIC S9(03) COMP-3  VALUE ZEROS.
       77  IND-SORT-SALVO              PIC S9(03) COMP-3  VALUE ZEROS.
       77  WRK-ULTIMO-CSEQ             PIC  9(03)         VALUE ZEROS.

       01  WRK-PRIM-ATRIB              PIC  X             VALUE  'S'.

       01  WRK-ATRIB-R.
           05  WRK-ATRIB               PIC 9(01)         VALUE ZEROS.
       01  WRK-MENSAGEM.
           03  FILLER                  PIC X(04)         VALUE SPACES.
           03  WRK-TRANSACAO           PIC X(08)         VALUE SPACES.
           03  FILLER                  PIC X(1988)       VALUE SPACES.

       01  WRK-COD-DEPTO.
           03  FILLER                  PIC 9(02)         VALUE ZEROS.
           03  WRK-COD-DEPTO-N         PIC 9(04)         VALUE ZEROS.

       01  WRK-INCONSIS                PIC X(01)         VALUE SPACES.

       01  WRK-TAB-MSG.
           05  WRK-MENSAGENS                  OCCURS  12    TIMES.
               07  WRK-MENSAG                 PIC 9(02).

       01  WRK-TAB-PROTEG.
           05  WRK-PROTEGE                    OCCURS  12    TIMES.
               07  WRK-PROT                   PIC X(01).

       01  WRK-TAB-IMOV-ORD.
           05  WRK-IMOVEIS                    OCCURS  25    TIMES.
               07  WRK-CMATR-IMOV             PIC X(010).
               07  WRK-IIMOV-RURAL            PIC X(025).
               07  WRK-CMUN-RURAL             PIC 9(09)     COMP-3.
               07  WRK-CINDCD-AREA-PRINC      PIC 9(001).
               07  WRK-CSEQ-IMOV-RURAL        PIC 9(003)    COMP-3.
               07  WRK-MAREA-IMOV-RURAL       PIC 9(007)V99 COMP-3.

      *----------------------------------------------------------------*
      ***  AREA  PARA  DATA  E  HORA  DO  SISTEMA
      *----------------------------------------------------------------*
       01  WRK-DT-SYS-POOL7600         PIC 9(08)         VALUE ZEROS.
       01  FILLER        REDEFINES     WRK-DT-SYS-POOL7600.
           03  WRK-AA-SYS7600          PIC 9(04).
           03  WRK-MM-SYS7600          PIC 9(02).
           03  WRK-DD-SYS7600          PIC 9(02).
       01  WRK-DT-SYS.
           03  WRK-DD-SYS              PIC 9(02)         VALUE ZEROS.
           03  WRK-BARRA1-SYS          PIC X(01)         VALUE SPACES.
           03  WRK-MM-SYS              PIC 9(02)         VALUE ZEROS.
           03  WRK-BARRA2-SYS          PIC X(01)         VALUE SPACES.
           03  WRK-AA-SYS              PIC 9(04)         VALUE ZEROS.

       01  WRK-TI-HHMMSS               PIC 9(06)         VALUE ZEROS.
       01  FILLER       REDEFINES      WRK-TI-HHMMSS.
           05  WRK-TI-HH               PIC 9(02).
           05  WRK-TI-MM               PIC 9(02).
           05  WRK-TI-SS               PIC 9(02).
       01  WRK-HORA-SYS.
           03  WRK-HH-SYS              PIC 9(02)         VALUE ZEROS.
           03  WRK-PONTO1-SYS          PIC X(01)         VALUE SPACES.
           03  WRK-MI-SYS              PIC 9(02)         VALUE ZEROS.
           03  WRK-PONTO2-SYS          PIC X(01)         VALUE SPACES.
           03  WRK-SS-SYS              PIC 9(02)         VALUE ZEROS.

       01  DATA-HORA.
           03  DT-JULIANA         PIC 9(005) COMP-3 VALUE ZEROS.
           03  DT-AAMMDD          PIC 9(007) COMP-3 VALUE ZEROS.
           03  DT-AAAAMMDD        PIC 9(009) COMP-3 VALUE ZEROS.
           03  TI-HHMMSS          PIC 9(007) COMP-3 VALUE ZEROS.
           03  TI-HHMMSSMMMMMM    PIC 9(013) COMP-3 VALUE ZEROS.
           03  TIMESTAMP          PIC X(020) VALUE SPACES.

      *----------------------------------------------------------------*
      ***  BOOK PARA O CAMPO WNAVG-CREDT-RURAL-TEXT DA TAB.RUECB042
      *----------------------------------------------------------------*

       COPY I#RUEC00.

      *----------------------------------------------------------------*
POL097***  AREA COMUNICACAO MODULO RUEC7722 PESQUISA IMOVEIS RURAIS
      *----------------------------------------------------------------*

       COPY I#RUEC22.

      *----------------------------------------------------------------*
POL097***  AREA COMUNICACAO MODULO CADU8430 PESQUISA CADU
      *----------------------------------------------------------------*

       COPY I#RUEC27.

POL097*  ** SUBSTITUICAO DAS INCLUDES CADU (SEM EMPRESA) **
POL097 COPY I#CADUL0.

      *---------------------------------------------------------------*
      ***          INCLUSAO DA POOL7100  -  TRATA ERRO
      *---------------------------------------------------------------*

       COPY POL7100C.

           EJECT
      *---------------------------------------------------------------*
      *** DEFINICAO DA AREA PARA RET-CODE DIFERENTES DE ZEROS(POOL0660)
      *---------------------------------------------------------------*

       01  WRK-ERRO-POOL0660.
           03  FILLER                  PIC X(36)         VALUE
               '* RETURN-CODE DIFERENTE DE ZEROS NA '.
           03  WRK-POOL                PIC X(08) VALUE SPACES.
           03  FILLER                  PIC X(03)         VALUE
               ' *'.
           03  FILLER                  PIC X(12)         VALUE
               '* RET.COD = '.
           03  WRK-RETURN-CODE         PIC 9(02)BB VALUE ZEROS.
           03  FILLER                  PIC X(08)         VALUE
               'LOCAL = '.
           03  WRK-LOCAL-ERRO          PIC 9(02)         VALUE ZEROS.
      *---------------------------------------------------------------*
      *** SQLCODES ESPECIFICOS
      *---------------------------------------------------------------*

      *---------------------------------------------------------------*
      ***                  DEFINICAO DE ATRIBUTOS
      *---------------------------------------------------------------*

       01  FILLER.
           03  TAB-ATRIBUTOS.

      *****************************************************************
      *    BRILHANTE, DESPROT, COM POS. CURSOR, NUM                   *
      * 1                                       ---                   *
               05  WRK-49369           PIC S9(008) COMP VALUE +49369.
      *****************************************************************
      *    NORMAL, DESPROT, COM POS. CURSOR, NUM                      *
      * 2                                    ---                      *
               05  WRK-49361           PIC S9(008) COMP VALUE +49361.
      *****************************************************************
      *    PROTEGIDO (NUMERICO OU ALFA)                               *
      * 3             -------- -- ----                                *
               05  WRK-225             PIC S9(008) COMP VALUE +225.
      *****************************************************************
      *    BRILHANTE, DESPROT, COM POS. CURSOR, ALFA                  *
      * 4                                       ----                  *
               05  WRK-49353           PIC S9(008) COMP VALUE +49353.
      *****************************************************************
      *    PROTEGIDO, BRILHANTE (NUMERICO OU ALFA)                    *
      * 5                        -------- -- ----                     *
               05  WRK-169             PIC S9(008) COMP VALUE +169.
      *****************************************************************
      *    PROTEGIDO, BRILHANTE, CURSOR (NUMERICO OU ALFA)            *
      * 6                                -------- -- ----             *
               05  WRK-49385           PIC S9(008) COMP VALUE +49385.
      *****************************************************************
      *    NORMAL, DESPROT, CURSOR, ALFA                              *
      * 7                           ----                              *
               05  WRK-49345           PIC S9(008) COMP VALUE +49345.
      *****************************************************************
      *    NORMAL, DESPROT, NUM                                       *
      * 8                                                             *
               05  WRK-209             PIC S9(008) COMP VALUE +209.
      *****************************************************************
      *    NORMAL, DESPROT, ALFA                                      *
      * 9                                                             *
               05  WRK-193             PIC S9(008) COMP VALUE +193.
      *****************************************************************

           03  FILLER REDEFINES TAB-ATRIBUTOS OCCURS 9 TIMES
                                INDEXED BY IND-ATRIB.
               05  TAB-ATRIBUTO        PIC S9(008) COMP.

           EJECT
      *---------------------------------------------------------------*
      ***                  TABELA DE MENSAGENS
      *---------------------------------------------------------------*

       01  FILLER.
           05  TAB-MENSAG.
               10  FILLER                   PIC X(79)         VALUE
               '7530-01 NAO EXISTEM DADOS PARA CONSULTA'.
               10  FILLER                   PIC X(79)         VALUE
               '7530-02 AMOSTRAGEM CONTINUA'.
               10  FILLER                   PIC X(79)         VALUE
               '7530-03 FINAL DE AMOSTRAGEM'.
               10  FILLER                   PIC X(79)         VALUE
               '7530-04 PF INVALIDA'.
               10  FILLER                   PIC X(79)         VALUE
               '7530-05 NAO EXISTE PAGINA ANTERIOR'.
               10  FILLER                   PIC X(79)         VALUE
               '7530-06 NAO EXISTE PAGINA POSTERIOR'.
               10  FILLER                   PIC X(79)         VALUE
               '7530-07 CODIGO DO TERMINAL EM BRANCO PARA SUBROTINA. AVI
      -        'SE DECRAM.'.
               10  FILLER                   PIC X(79)         VALUE
POL097         '7530-08 CCLUB RUEC NAO PREENCHIDO. AVISE DECRAM.'.
               10  FILLER                   PIC X(79)         VALUE
               '7530-09 CODIGO PRODUTO NAO PREENCHIDO. AVISE DECRAM.'.
               10  FILLER                   PIC X(79)         VALUE
               '7530-10 CODIGO ATIVIDADE NAO PREENCHIDO. AVISE DECRAM.'.
               10  FILLER                   PIC X(79)         VALUE
               '7530-11 CODIGO ATIVIDADE FORA DO PREVISTO. AVISE DECRAM.
      -        ' '.
               10  FILLER                   PIC X(79)         VALUE
               '7530-12 TIPO DE EXPLORACAO NAO PREENCHIDO. AVISE DECRAM.
      -        ' '.
               10  FILLER                   PIC X(79)         VALUE
               '7530-13 TIPO DE EXPLORACAO DO IMOVEL DIFERENTE DO IMOVEL
      -        'INFORMADO.'.
               10  FILLER                   PIC X(79)         VALUE
               '7530-14 CULTURA PARA FINANCIAMENTO SOLICITADO NAO EXISTE
      -        'NO IMOVEL INFORMADO.'.
               10  FILLER                   PIC X(79)         VALUE
               '7530-15 TIPO DE EXPLORACAO FORA DO PREVISTO. AVISE DECRA
      -        'M.'.
               10  FILLER                   PIC X(79)         VALUE
               '7530-16 REBANHO PARA FINANCIAMENTO SOLICITADO NAO EXISTE
      -        'NO IMOVEL INFORMADO.'.
               10  FILLER                   PIC X(79)         VALUE
               '7530-17 REBANHO CADASTRADO EM DUPLICIDADE PARA O IMOVEL
      -        'INFORMADO.'.
               10  FILLER                   PIC X(79)         VALUE
               '7530-18 FINAL DE PESQUISA DOS IMOVEIS DO PROPONENTE'.
               10  FILLER                   PIC X(79)         VALUE
               '7530-19 PESQUISA EFETUADA. TECLE PF8 PARA PROXIMA TELA'.
               10  FILLER                   PIC X(79)         VALUE
               '7530-20 NAO EXISTEM IMOVEIS COM O TIPO DE EXPLORACAO / P
      -        'RODUTO INFORMADOS.'.
               10  WRK-MSG21-TAB            PIC X(79).
               10  WRK-MSG22-TAB            PIC X(79).
               10  WRK-MSG23-TAB            PIC X(79).
               10  FILLER                   PIC X(79)         VALUE
               '7530-24 INDICAR SOMENTE UM IMOVEL COMO AREA PRINCIPAL'.
               10  FILLER                   PIC X(79)         VALUE
               '7530-25 LIMITE DE 25 IMOVEIS EXCEDIDO. AVISE DECRAM.'.
               10  FILLER                   PIC X(79)         VALUE
               '7530-26 CARACTER INVALIDO'.
               10  FILLER                   PIC X(79)         VALUE
               '7530-27 SELECAO/RETIRADA DE IMOVEIS EFETUADA COM SUCESSO
      -        '. TECLE PF11.'.
               10  FILLER                   PIC X(79)         VALUE
               '7530-28 NENHUM IMOVEL FOI SELECIONADO PARA O FINANCIAMEN
      -        'TO'.
               10  FILLER                   PIC X(79)         VALUE
               '7530-29 INDICAR QUAL IMOVEL EH O PRINCIPAL'.
               10  FILLER                   PIC X(79)         VALUE
               '7530-30 POSICIONE O CURSOR EM ALGUM IMOVEL PARA EFETUAR
      -        'CONSULTA'.
               10  FILLER                   PIC X(79)         VALUE
               '7530-31 PESQUISA EFETUADA. NAO HA MAIS PAGINAS PARA AVAN
      -        'CAR'.
               10  FILLER                   PIC X(79)         VALUE
               '7530-32 PESQUISA EFETUADA. NAO HA MAIS PAGINAS PARA RETR
      -        'OCEDER'.
POL097         10  FILLER                   PIC X(79)         VALUE
POL097         '7530-33 INCONSISTENCIA CHAMADA MODULO RUEC7722 - PESQ. I
POL097-        'MOVEIS'.
POL097         10  FILLER                   PIC X(79)         VALUE
POL097         '7530-34 IMOVEL CADASTRADO NO RUEC FOI EXCLUIDO DO CADU'.
POL097         10  FILLER                   PIC X(79)         VALUE
POL097         '7530-35 INCONSISTENCIA CHAMADA MODULO CADU8430 - PESQ. I
POL097-        'MOVEIS'.
POL097         10  FILLER                   PIC X(79)         VALUE
POL097         '7530-36 IMOVEL CADU NAO ENCONTRADO NO MODULO CADU8430'.
EG0617         10  FILLER                   PIC X(79)         VALUE
EG0617         '7530-37 SEQUENCIA DO IMOVEL NAO ENCONTRADA'.
EG0617         10  FILLER                   PIC X(79)         VALUE
EG0617         '7530-38 IMOVEL NAO POSSUI CCIR, NIRF OU NRO.CAR NO CADAS
EG0617-        'TRO RURAL'.
CAR            10  FILLER                   PIC X(79)         VALUE
CAR            '7530-39 PARA ESTE EMPREENDIMENTO, SAO PERMITIDOS SOMENTE
CAR   -        ' IMOVEIS COM NRO. CAR. '.
STEFAB         10  FILLER                   PIC X(79)         VALUE
.              'FICHA CADASTRAL NAO CONSTA CODIGO NIRF E CCIR, ATUALIZAR
.     -        ' INFORMACOES. '.
STEFAB     05  TAB-MENSAG-R REDEFINES TAB-MENSAG OCCURS 40 INDEXED BY
               IND-MENSAG.
               10  ELE-MENSAG               PIC X(79).

      /----------------------------------------------------------------*
      ***  TELA RUECT753            - INPUT
      *----------------------------------------------------------------*
       01  INPUT-T753.
           03  FILLER                       PIC  X(18) VALUE SPACES.
           03  INP-PFK-T753                 PIC  X(01) VALUE SPACES.
           03  INP-SENHAS-T753              PIC  X(37) VALUE SPACES.
           03  INP-NOPCAO-T753              PIC  X(02) VALUE SPACES.
           03  INP-POSI-T753.
               05  INP-POSI-LINHA-T753      PIC  9(04) COMP VALUE ZEROS.
               05  INP-POSI-COLUNA-T753     PIC  9(04) COMP VALUE ZEROS.
           03  INP-DATA-T753                PIC  X(10) VALUE SPACES.
           03  INP-HORA-T753                PIC  X(08) VALUE SPACES.
           03  INP-EXPLO-T753               PIC  X(20) VALUE SPACES.
           03  INP-PROTEG-T753              PIC  X(12) VALUE SPACES.
           03  INP-PAG-T753.
               07  INP-PAG-T753-R           PIC  9(03) VALUE ZEROS.
           03  INP-FIMAMOS-T753             PIC  X(01) VALUE SPACES.
           03  INP-TERMINAL-T753            PIC  X(08) VALUE SPACES.
           03  INP-FASE-T753                PIC  X(01) VALUE SPACES.
POL097     03  INP-CCLUB-T753               PIC  9(10) VALUE ZEROS.
           03  INP-QTPRINC-T753-X.
               07  INP-QTPRINC-T753         PIC  9(02) VALUE ZEROS.
           03  INP-QTSEL-T753-X.
               07  INP-QTSEL-T753           PIC  9(02) VALUE ZEROS.
           03  INP-TABELA-T753    OCCURS    012  TIMES.
               05  INP-SEL-T753             PIC  X(01).
               05  INP-DENO-T753            PIC  X(25).
POL097         05  INP-MATRI-T753           PIC  X(10).
               05  INP-MUNI-T753            PIC  X(15).
               05  INP-UF-T753              PIC  X(02).
               05  INP-STATUS-T753          PIC  X(21).
           03  INP-MENSAG-T753              PIC  X(79) VALUE SPACES.

      /----------------------------------------------------------------*
      ***  TELA RUECT753            - OUTPUT
      *----------------------------------------------------------------*
       01  OUTPUT-T753.
POL097     03  OUT-LL-T753                  PIC S9(04) COMP VALUE +1111.
           03  OUT-ZZ-T753                  PIC  9(04) COMP VALUE  0.
           03  OUT-DADOS-T753.
           05  OUT-SENHAS-T753              PIC  X(37) VALUE SPACES.
           05  OUT-NOPCAO-T753              PIC  X(02) VALUE SPACES.
           05  OUT-DATA-T753                PIC  X(10) VALUE SPACES.
           05  OUT-HORA-T753                PIC  X(08) VALUE SPACES.
           05  OUT-EXPLO-T753               PIC  X(20) VALUE SPACES.
           05  OUT-PROTEG-T753              PIC  X(12) VALUE SPACES.
           05  OUT-PAG-T753.
               07  OUT-PAG-T753-R           PIC  9(03) VALUE ZEROS.
           05  OUT-FIMAMOS-T753             PIC  X(01) VALUE SPACES.
           05  OUT-TERMINAL-T753            PIC  X(08) VALUE SPACES.
           05  OUT-FASE-T753                PIC  X(01) VALUE SPACES.
POL097     05  OUT-CCLUB-T753               PIC  9(10) VALUE ZEROS.
           05  OUT-QTPRINC-T753-X.
               07  OUT-QTPRINC-T753         PIC  9(02) VALUE ZEROS.
           05  OUT-QTSEL-T753-X.
               07  OUT-QTSEL-T753           PIC  9(02) VALUE ZEROS.
           05  OUT-TABELA-T753    OCCURS    012  TIMES.
               07  OUT-SEL-ATTR-T753        PIC  9(04) COMP.
               07  OUT-SEL-T753             PIC  X(01).
               07  OUT-DENO-T753            PIC  X(25).
POL097         07  OUT-MATRI-T753           PIC  X(10).
               07  OUT-MUNI-T753            PIC  X(15).
               07  OUT-UF-T753              PIC  X(02).
               07  OUT-STATUS-T753          PIC  X(21).
           05  OUT-MENSAG-T753              PIC  X(79) VALUE SPACES.

      /----------------------------------------------------------------*
      ***  AREA DA POOL0660 COM MASCARA DA TELA RUECT753 - SEGMENTO 00
      *----------------------------------------------------------------*
       01  660-RUECT753.
           03  660-LL-AREA-T753             PIC  9(04) COMP VALUE 0176.
POL097     03  660-LL-MENSAGEM-T753         PIC  9(04) COMP VALUE 1111.
           03  660-SENHAS-T753              PIC  9(04) COMP VALUE 0037.
           03  660-NOPCAO-T753              PIC  9(04) COMP VALUE 1002.
           03  660-DATA-T753                PIC  9(04) COMP VALUE 0010.
           03  660-HORA-T753                PIC  9(04) COMP VALUE 0008.
           03  660-EXPLO-T753               PIC  9(04) COMP VALUE 0020.
           03  660-PROTEG-T753              PIC  9(04) COMP VALUE 0012.
           03  660-PAG-T753                 PIC  9(04) COMP VALUE 0003.
           03  660-FIMAMOS-T753             PIC  9(04) COMP VALUE 0001.
           03  660-TERMINAL-T753            PIC  9(04) COMP VALUE 0008.
           03  660-FASE-T753                PIC  9(04) COMP VALUE 0001.
POL097     03  660-CCLUB-T753               PIC  9(04) COMP VALUE 0010.
           03  660-QTPRINC-T753             PIC  9(04) COMP VALUE 0002.
           03  660-QTSEL-T753               PIC  9(04) COMP VALUE 0002.
           03  660-SEL-0001-T753            PIC  9(04) COMP VALUE 2003.
           03  660-DENO-0001-T753           PIC  9(04) COMP VALUE 0025.
           03  660-MATRI-0001-T753          PIC  9(04) COMP VALUE 0010.
           03  660-MUNI-0001-T753           PIC  9(04) COMP VALUE 0015.
           03  660-UF-0001-T753             PIC  9(04) COMP VALUE 0002.
           03  660-STATUS-0001-T753         PIC  9(04) COMP VALUE 0021.
           03  660-SEL-0002-T753            PIC  9(04) COMP VALUE 2003.
           03  660-DENO-0002-T753           PIC  9(04) COMP VALUE 0025.
           03  660-MATRI-0002-T753          PIC  9(04) COMP VALUE 0010.
           03  660-MUNI-0002-T753           PIC  9(04) COMP VALUE 0015.
           03  660-UF-0002-T753             PIC  9(04) COMP VALUE 0002.
           03  660-STATUS-0002-T753         PIC  9(04) COMP VALUE 0021.
           03  660-SEL-0003-T753            PIC  9(04) COMP VALUE 2003.
           03  660-DENO-0003-T753           PIC  9(04) COMP VALUE 0025.
           03  660-MATRI-0003-T753          PIC  9(04) COMP VALUE 0010.
           03  660-MUNI-0003-T753           PIC  9(04) COMP VALUE 0015.
           03  660-UF-0003-T753             PIC  9(04) COMP VALUE 0002.
           03  660-STATUS-0003-T753         PIC  9(04) COMP VALUE 0021.
           03  660-SEL-0004-T753            PIC  9(04) COMP VALUE 2003.
           03  660-DENO-0004-T753           PIC  9(04) COMP VALUE 0025.
           03  660-MATRI-0004-T753          PIC  9(04) COMP VALUE 0010.
           03  660-MUNI-0004-T753           PIC  9(04) COMP VALUE 0015.
           03  660-UF-0004-T753             PIC  9(04) COMP VALUE 0002.
           03  660-STATUS-0004-T753         PIC  9(04) COMP VALUE 0021.
           03  660-SEL-0005-T753            PIC  9(04) COMP VALUE 2003.
           03  660-DENO-0005-T753           PIC  9(04) COMP VALUE 0025.
           03  660-MATRI-0005-T753          PIC  9(04) COMP VALUE 0010.
           03  660-MUNI-0005-T753           PIC  9(04) COMP VALUE 0015.
           03  660-UF-0005-T753             PIC  9(04) COMP VALUE 0002.
           03  660-STATUS-0005-T753         PIC  9(04) COMP VALUE 0021.
           03  660-SEL-0006-T753            PIC  9(04) COMP VALUE 2003.
           03  660-DENO-0006-T753           PIC  9(04) COMP VALUE 0025.
           03  660-MATRI-0006-T753          PIC  9(04) COMP VALUE 0010.
           03  660-MUNI-0006-T753           PIC  9(04) COMP VALUE 0015.
           03  660-UF-0006-T753             PIC  9(04) COMP VALUE 0002.
           03  660-STATUS-0006-T753         PIC  9(04) COMP VALUE 0021.
           03  660-SEL-0007-T753            PIC  9(04) COMP VALUE 2003.
           03  660-DENO-0007-T753           PIC  9(04) COMP VALUE 0025.
           03  660-MATRI-0007-T753          PIC  9(04) COMP VALUE 0010.
           03  660-MUNI-0007-T753           PIC  9(04) COMP VALUE 0015.
           03  660-UF-0007-T753             PIC  9(04) COMP VALUE 0002.
           03  660-STATUS-0007-T753         PIC  9(04) COMP VALUE 0021.
           03  660-SEL-0008-T753            PIC  9(04) COMP VALUE 2003.
           03  660-DENO-0008-T753           PIC  9(04) COMP VALUE 0025.
           03  660-MATRI-0008-T753          PIC  9(04) COMP VALUE 0010.
           03  660-MUNI-0008-T753           PIC  9(04) COMP VALUE 0015.
           03  660-UF-0008-T753             PIC  9(04) COMP VALUE 0002.
           03  660-STATUS-0008-T753         PIC  9(04) COMP VALUE 0021.
           03  660-SEL-0009-T753            PIC  9(04) COMP VALUE 2003.
           03  660-DENO-0009-T753           PIC  9(04) COMP VALUE 0025.
           03  660-MATRI-0009-T753          PIC  9(04) COMP VALUE 0010.
           03  660-MUNI-0009-T753           PIC  9(04) COMP VALUE 0015.
           03  660-UF-0009-T753             PIC  9(04) COMP VALUE 0002.
           03  660-STATUS-0009-T753         PIC  9(04) COMP VALUE 0021.
           03  660-SEL-0010-T753            PIC  9(04) COMP VALUE 2003.
           03  660-DENO-0010-T753           PIC  9(04) COMP VALUE 0025.
           03  660-MATRI-0010-T753          PIC  9(04) COMP VALUE 0010.
           03  660-MUNI-0010-T753           PIC  9(04) COMP VALUE 0015.
           03  660-UF-0010-T753             PIC  9(04) COMP VALUE 0002.
           03  660-STATUS-0010-T753         PIC  9(04) COMP VALUE 0021.
           03  660-SEL-0011-T753            PIC  9(04) COMP VALUE 2003.
           03  660-DENO-0011-T753           PIC  9(04) COMP VALUE 0025.
           03  660-MATRI-0011-T753          PIC  9(04) COMP VALUE 0010.
           03  660-MUNI-0011-T753           PIC  9(04) COMP VALUE 0015.
           03  660-UF-0011-T753             PIC  9(04) COMP VALUE 0002.
           03  660-STATUS-0011-T753         PIC  9(04) COMP VALUE 0021.
           03  660-SEL-0012-T753            PIC  9(04) COMP VALUE 2003.
           03  660-DENO-0012-T753           PIC  9(04) COMP VALUE 0025.
           03  660-MATRI-0012-T753          PIC  9(04) COMP VALUE 0010.
           03  660-MUNI-0012-T753           PIC  9(04) COMP VALUE 0015.
           03  660-UF-0012-T753             PIC  9(04) COMP VALUE 0002.
           03  660-STATUS-0012-T753         PIC  9(04) COMP VALUE 0021.
           03  660-MENSAG-T753              PIC  9(04) COMP VALUE 0079.

      *----------------------------------------------------------------*
      *        AREA DE RECEBIMENTO DE CONTROLE DO PROGRAMA RUEC7000    *
      *----------------------------------------------------------------*

       01  INP-RUEC7000.
           03 INP-LL-7000              PIC S9(04) COMP VALUE ZEROS.
           03 INP-ZZ-7000              PIC  9(04) COMP VALUE ZEROS.
           03 INP-TRANCODE-7000        PIC  X(15) VALUE SPACES.
           03 INP-SENHA-7000           PIC  X(37) VALUE SPACES.
           03 INP-NVOP-7000            PIC  X(02) VALUE SPACES.
           03 INP-CODIGO-TERMINAL-7000 PIC  X(08) VALUE SPACES.
           03 INP-STATUS-RETORNO-7000  PIC  X(02)      VALUE SPACES.
           03 INP-MENSAGEM-7000        PIC  X(79)      VALUE SPACES.
           03 INP-PAGINA-7000          PIC  9(03) VALUE ZEROS.


      *----------------------------------------------------------------*
      *      AREA DE PASSAGEM DE CONTROLE PARA O PROGRAMA RUEC7000     *
      *----------------------------------------------------------------*

       01  OUT-RUEC7000.
           03 OUT-LL-7000              PIC S9(04) COMP VALUE +147.
           03 OUT-ZZ-7000              PIC  9(04) COMP VALUE  0.
           03 OUT-TRANCODE-7000        PIC  X(15)      VALUE 'RUEC7000'.
           03 OUT-SENHA-7000           PIC  X(37) VALUE SPACES.
           03 OUT-NVOP-7000            PIC  9(02)      VALUE ZEROS.
           03 OUT-CODIGO-TERMINAL-7000 PIC  X(08)      VALUE SPACES.
           03 OUT-STATUS-RETORNO-7000  PIC  X(02)      VALUE SPACES.
           03 OUT-MENSAGEM-7000        PIC  X(79)      VALUE SPACES.

      *---------------------------------------------------------------*
      ***                       TABELAS DB2
      *---------------------------------------------------------------*

           EXEC SQL
               INCLUDE  SQLCA
           END-EXEC.

POL097     EXEC SQL
POL097         INCLUDE  CADUB089
POL097     END-EXEC.

           EXEC SQL
               INCLUDE  RUECB042
           END-EXEC.

POL097     EXEC SQL
POL097         INCLUDE  RUECB051
POL097     END-EXEC.

POL097     EXEC SQL
POL097         INCLUDE  RUECB055
POL097     END-EXEC.

POL097     EXEC SQL
POL097         INCLUDE  RUECB056
POL097     END-EXEC.

POL097     EXEC SQL
POL097         INCLUDE  RUECB060
POL097     END-EXEC.

POL097     EXEC SQL
POL097         INCLUDE  RUECB063
POL097     END-EXEC.

           EXEC SQL
               INCLUDE  RURCV010
           END-EXEC.

POL097     EXEC SQL
POL097       DECLARE CV51  CURSOR  FOR
POL097       SELECT
POL097             CPRODT_RURAL
POL097       FROM
POL097              DB2PRD.TCULTR_AGRIC_IMOV
POL097       WHERE
POL097             CCLUB              = :RUECB051.CCLUB  AND
POL097             CSEQ_IMOV_RURAL    = :RUECB051.CSEQ-IMOV-RURAL AND
POL097             HULT_ATULZ_EFETU   < :RUECB051.HULT-ATULZ-EFETU
POL097     END-EXEC.

POL097     EXEC SQL
POL097       DECLARE CV60  CURSOR  FOR
POL097       SELECT
POL097             CPRODT_RURAL
POL097       FROM
POL097              DB2PRD.TRBNHO_IMOV_RURAL
POL097       WHERE
POL097             CCLUB              = :RUECB060.CCLUB  AND
POL097             CSEQ_IMOV_RURAL    = :RUECB060.CSEQ-IMOV-RURAL AND
POL097             HULT_ATULZ_EFETU   < :RUECB060.HULT-ATULZ-EFETU
POL097     END-EXEC.

      *---------------------------------------------------------------*
       LINKAGE SECTION.
      *---------------------------------------------------------------*

       01  IO-PCB.
           03  IO-LTERM                PIC X(08).
           03  FILLER                  PIC X(02).
           03  IO-STATUS               PIC X(02).
           03  FILLER                  PIC X(12).
           03  IO-MODNAME              PIC X(08).

       01  ALT-PCB.
           03  ALT-LTERM               PIC X(08).
           03  FILLER                  PIC X(02).
           03  ALT-STATUS              PIC X(02).
           03  FILLER                  PIC X(12).
           03  ALT-MODNAME             PIC X(08).

           EJECT
      *===============================================================*
       PROCEDURE DIVISION.
      *===============================================================*

           ENTRY       'DLITCBL'       USING   IO-PCB ALT-PCB.

           MOVE        WRK-GU          TO      WRK-FUNCAO.

      *---------------------------------------------------------------*
       00000-OBTER-MENSAGEM SECTION.
      *---------------------------------------------------------------*

           CALL    'POOL5000'      USING       WRK-FUNCAO
                                               IO-PCB
                                               ALT-PCB
                                               WRK-MENSAGEM
                                               WRK-TELA
                                               WRK-NOVA-OPCAO
                                               WRK-COD-USER
                                               WRK-OPCAO-ATUAL
                                               WRK-COD-DEPTO.

           IF  RETURN-CODE EQUAL 04 OR 12
               GOBACK.

           MOVE        SPACES           TO   OUT-DADOS-T753
                                             WRK-PGM-CHAMADOR
                                             WRK-ERRO
           MOVE        ZEROS            TO   WRK-QT-DESPREZAR
                                             WRK-PASSAR-CONTROLE
                                             WRK-TEM-REGISTROS
                                             WRK-ACHEI-CSEQ-IMOV
                                             WRK-QTDE-IMOV-PRINC
                                             WRK-QTSEL
                                             WRK-QTPRINC
                                             WRK-MSG
                                             WRK-ATRIB
           INITIALIZE                        WRK-TAB-PROTEG
           INITIALIZE                        WRK-TAB-MSG

           CALL   'POOL7600'    USING   DATA-HORA

           IF  RETURN-CODE  NOT EQUAL  ZEROS
               MOVE 'APL'               TO   ERR-TIPO-ACESSO
               MOVE 'POOL7600'          TO   WRK-POOL
               MOVE RETURN-CODE         TO   WRK-RETURN-CODE
               MOVE 1                   TO   WRK-LOCAL-ERRO
               MOVE WRK-ERRO-POOL0660   TO   ERR-TEXTO
               PERFORM 9999-ROTINA-ERRO.

           MOVE    DT-AAAAMMDD          TO    WRK-DT-SYS-POOL7600
           MOVE    WRK-AA-SYS7600       TO    WRK-AA-SYS
           MOVE    WRK-MM-SYS7600       TO    WRK-MM-SYS
           MOVE    WRK-DD-SYS7600       TO    WRK-DD-SYS
           MOVE    '/'                  TO    WRK-BARRA1-SYS
                                              WRK-BARRA2-SYS
           MOVE    WRK-DT-SYS           TO    OUT-DATA-T753

           MOVE    TI-HHMMSS            TO    WRK-TI-HHMMSS
           MOVE    WRK-TI-HH            TO    WRK-HH-SYS
           MOVE    WRK-TI-MM            TO    WRK-MI-SYS
           MOVE    WRK-TI-SS            TO    WRK-SS-SYS
           MOVE    ':'                  TO    WRK-PONTO1-SYS
                                              WRK-PONTO2-SYS
           MOVE    WRK-HORA-SYS         TO    OUT-HORA-T753

           PERFORM 10000-PROCESSAR-MSG.

           GO TO 00000-OBTER-MENSAGEM.

      *---------------------------------------------------------------*
       00000-99-FIM. EXIT.
      *---------------------------------------------------------------*
           EJECT
      *---------------------------------------------------------------*
       10000-PROCESSAR-MSG SECTION.
      *---------------------------------------------------------------*

           MOVE  'RUECT753'                  TO    WRK-TELA

           IF WRK-TRANSACAO     NOT EQUAL     'RUEC7530'
              PERFORM 11000-RECEBER-CONTROLE
           ELSE
              PERFORM 12000-PROCESSAR-RUEC7530.

           IF  WRK-TELA  EQUAL 'RUECT753'
               PERFORM 15000-POOL0660-RUECT753.

      *---------------------------------------------------------------*
       10000-99-FIM. EXIT.
      *---------------------------------------------------------------*

      *----------------------------------------------------------------*
       10500-PASSAR-CONTROLE    SECTION.
      *----------------------------------------------------------------*
           IF  WNAV-FUNCAO        EQUAL 'A'
STEFAB         MOVE 'RUEC8000'    TO WRK-PGM-CHAMADOR
STEFAB         MOVE 'RUEC7530'    TO PROGRAMA-ANT
STEFAB     ELSE
STEFAB         MOVE 'RUEC7000'    TO WRK-PGM-CHAMADOR
STEFAB         MOVE 'RUEC7530'    TO PROGRAMA-ANT
STEFAB     END-IF.

           PERFORM  12850-REGRAVAR-RUECB042.
           MOVE    'RUEC7530'           TO   OUT-TRANCODE-7000
           MOVE    WRK-PGM-CHAMADOR     TO   WRK-TELA
           MOVE    +147                 TO   OUT-LL-7000
           MOVE    IO-LTERM             TO   OUT-CODIGO-TERMINAL-7000
           MOVE    OUT-RUEC7000         TO   WRK-MENSAGEM
           MOVE    WRK-CHNG             TO   WRK-FUNCAO.

           MOVE    1                    TO   WRK-PASSAR-CONTROLE.

      *----------------------------------------------------------------*
       10500-99-FIM. EXIT.
      *----------------------------------------------------------------*

      *****************************************************************
      *  ESSA ROTINA INSERE A TELA QUANDO O PGM RECEBE O CONTROLE     *
      *****************************************************************

      *---------------------------------------------------------------*
       11000-RECEBER-CONTROLE SECTION.
      *---------------------------------------------------------------*

           MOVE   WRK-MENSAGEM               TO    INP-RUEC7000

           IF     INP-CODIGO-TERMINAL-7000   EQUAL  SPACES
                  MOVE    ELE-MENSAG(07)     TO     OUT-MENSAGEM-7000
                  PERFORM 14900-FINALIZA-VALIDACAO
                  GO     TO     11000-99-FIM.

           MOVE   INP-CODIGO-TERMINAL-7000   TO    CTERM OF RUECB042
                                                   OUT-TERMINAL-T753
                                                   INP-TERMINAL-T753

           MOVE   ZEROS                      TO    OUT-PAG-T753
                                                   INP-PAG-T753

           PERFORM 12840-ACESSA-RUECB042

           PERFORM    11330-PROTEGER
             VARYING  IND    FROM   1   BY   1
             UNTIL    IND    GREATER   12

           IF  WRK-TRANSACAO     EQUAL     'RUEC7580'
               IF  INP-STATUS-RETORNO-7000     EQUAL    SPACES
                   PERFORM 11210-RESTAURA-TELA
               ELSE
                   MOVE  INP-MENSAGEM-7000   TO  OUT-MENSAG-T753
           ELSE
               PERFORM 11100-VALIDAR-DADOS-RECEBIDOS
               IF  WRK-PASSAR-CONTROLE    EQUAL    0
                   PERFORM 11200-VALIDAR-DENO-IMOVEL.

      *---------------------------------------------------------------*
       11000-99-FIM. EXIT.
      *---------------------------------------------------------------*

      *---------------------------------------------------------------*
       11100-VALIDAR-DADOS-RECEBIDOS        SECTION.
      *---------------------------------------------------------------*

           MOVE    'ER'        TO      OUT-STATUS-RETORNO-7000
           MOVE    SPACES      TO      OUT-MENSAGEM-7000

POL097     IF  CCLUB-RUEC          EQUAL   ZEROS
               MOVE    ELE-MENSAG(08)    TO   OUT-MENSAGEM-7000
               PERFORM 14900-FINALIZA-VALIDACAO
               GO     TO     11100-99-FIM
POL097     END-IF.

           IF  COD-PRODUTO        EQUAL   ZEROS
               MOVE    ELE-MENSAG(09)    TO   OUT-MENSAGEM-7000
               PERFORM 14900-FINALIZA-VALIDACAO
               GO     TO     11100-99-FIM.

           IF  COD-ATIVIDADE      EQUAL   ZEROS
               MOVE    ELE-MENSAG(10)    TO   OUT-MENSAGEM-7000
               PERFORM 14900-FINALIZA-VALIDACAO
               GO     TO     11100-99-FIM.

           IF  COD-ATIVIDADE  NOT EQUAL 1 AND 2 AND 3 AND 4 AND 5 AND 6
CCM01A                                    AND 999
               MOVE    ELE-MENSAG(11)    TO   OUT-MENSAGEM-7000
               PERFORM 14900-FINALIZA-VALIDACAO
               GO     TO     11100-99-FIM.

           IF  TIPO-EXPLOR-RURAL  EQUAL SPACES
               MOVE    ELE-MENSAG(12)    TO   OUT-MENSAGEM-7000
               PERFORM 14900-FINALIZA-VALIDACAO
               GO     TO     11100-99-FIM.

           IF  TIPO-EXPLOR-RURAL NOT EQUAL 'PROPRIA'  AND 'ARRENDAMENTO'
                                       AND 'COMODATO' AND 'PARCERIA'
                                       AND 'MEEIRO'   AND 'CONDOMINIO'
POL097                                 AND 'USUFRUTO'
               MOVE    ELE-MENSAG(15)    TO   OUT-MENSAGEM-7000
               PERFORM 14900-FINALIZA-VALIDACAO
               GO     TO     11100-99-FIM.

      *---------------------------------------------------------------*
       11100-99-FIM. EXIT.
      *---------------------------------------------------------------*

      *---------------------------------------------------------------*
       11200-VALIDAR-DENO-IMOVEL    SECTION.
      *---------------------------------------------------------------*

POL097     PERFORM  11255-ACESSA-RUECB063.

           IF  IIMOV-RURAL OF NAVEGACAO-RURAL (01)  EQUAL   SPACES
               PERFORM 11287-ABRE-LISTA
               GO     TO     11200-99-FIM.

POL097     PERFORM 11230-SELECT-RUECB056
           IF  SQLCODE     EQUAL   +100 OR -811
               PERFORM 11287-ABRE-LISTA
               GO     TO     11200-99-FIM
           ELSE
POL097         IF  RTPO-EXPLO-IMOV   OF RUECB056    NOT  EQUAL
                   TIPO-EXPLOR-RURAL OF NAVEGACAO-RURAL
                   PERFORM 11287-ABRE-LISTA
                   GO     TO     11200-99-FIM
               ELSE
                   PERFORM 11220-VERIFICA-PRODUTO.

      *---------------------------------------------------------------*
       11200-99-FIM. EXIT.
      *---------------------------------------------------------------*

      *---------------------------------------------------------------*
       11210-RESTAURA-TELA                SECTION.
      *---------------------------------------------------------------*

           MOVE    'S'       TO    WRK-PRIM-ATRIB.

           PERFORM  11215-MOVE-RESTAURA-TELA
                    VARYING  IND    FROM  1 BY 1
                    UNTIL    IND    GREATER   12

           MOVE  CCLUB-RUEC          TO   INP-CCLUB-T753
                                          OUT-CCLUB-T753.

           MOVE   HOLD-7530-PAGINA
           TO     OUT-PAG-T753-R

           MOVE   HOLD-7530-TIPO-EXPLOR-RURAL
           TO     OUT-EXPLO-T753
           MOVE   HOLD-7530-MSG   OF  HOLD-TELA-RUEC7530
           TO     OUT-MENSAG-T753

           MOVE    SPACES          TO   AREA-SALVA-TELA1
           PERFORM  12850-REGRAVAR-RUECB042.

      *---------------------------------------------------------------*
       11210-99-FIM. EXIT.
      *---------------------------------------------------------------*

      *----------------------------------------------------------------*
       11215-MOVE-RESTAURA-TELA       SECTION.
      *----------------------------------------------------------------*
           IF  HOLD-7530-DENOMINACAO(IND)   EQUAL     SPACES
               MOVE   12    TO      IND
               GO     TO     11215-99-FIM.

           MOVE    HOLD-7530-SELECIONA  (IND)  TO  OUT-SEL-T753   (IND).
           MOVE    HOLD-7530-DENOMINACAO(IND)  TO  OUT-DENO-T753  (IND).
           MOVE    HOLD-7530-MATRICULA  (IND)  TO  OUT-MATRI-T753 (IND).
           MOVE    HOLD-7530-MUNICIPIO  (IND)  TO  OUT-MUNI-T753  (IND).
           MOVE    HOLD-7530-UF         (IND)  TO  OUT-UF-T753    (IND).
           MOVE    HOLD-7530-STATUS     (IND)  TO  OUT-STATUS-T753(IND).

      *----desproteger ocorrencia-------------------------------------*

           IF HOLD-7530-DESPROTEGE-SEL(IND)   EQUAL  'S'
              MOVE TAB-ATRIBUTO (09)   TO     OUT-SEL-ATTR-T753 (IND)
              IF   WRK-PRIM-ATRIB    EQUAL    'S'
                  MOVE TAB-ATRIBUTO (07)   TO  OUT-SEL-ATTR-T753 (IND)
                  MOVE  'N'  TO  WRK-PRIM-ATRIB.

      *----------------------------------------------------------------*
       11215-99-FIM. EXIT.
      *----------------------------------------------------------------*

      *---------------------------------------------------------------*
       11220-VERIFICA-PRODUTO      SECTION.
      *---------------------------------------------------------------*

      *----------------------------------------- AGRICULTURA ---------*
           IF  COD-ATIVIDADE     EQUAL   1   OR   3   OR   5
               PERFORM 11240-ACESSA-RUECB051
               IF  SQLCODE     EQUAL   +100
                   PERFORM 11287-ABRE-LISTA
                   GO     TO     11220-99-FIM
               ELSE
POL097            PERFORM 11260-ACESSA-IMOVEL
                  PERFORM 11265-CARREGA-CAMPOS
           ELSE
      *----------------------------------------- PECUARIA ------------*
               PERFORM 11250-ACESSA-RUECB060
               IF  SQLCODE     EQUAL   +100
                   PERFORM 11287-ABRE-LISTA
                   GO     TO     11220-99-FIM
               ELSE
POL097             PERFORM 11260-ACESSA-IMOVEL
                   PERFORM 11265-CARREGA-CAMPOS.

      *---------------------------------------------------------------*
       11220-99-FIM. EXIT.
      *---------------------------------------------------------------*

      *---------------------------------------------------------------*
POL097 11230-SELECT-RUECB056   SECTION.
      *---------------------------------------------------------------*

POL097     MOVE    HULT-EFTVC-ALT OF RUECB063
POL097                               TO   HULT-ATULZ-EFETU OF RUECB056
           MOVE    IIMOV-RURAL       OF   NAVEGACAO-RURAL (01)
POL097     TO      RIMOV-RURAL       OF   RUECB056
POL097     MOVE    CCLUB-RUEC        TO   CCLUB
POL097                                    OF RUECB056
           EXEC SQL
             SELECT
                   CSEQ_IMOV_RURAL,
POL097             MAREA_CULTR_AGRIC,
POL097             MAREA_PASTA,
POL097             MDMAIS_AREA,
POL097             RTPO_EXPLO_IMOV,
POL097             CSEQ_PATRM_IMOV
             INTO
POL097             :RUECB056.CSEQ-IMOV-RURAL,
POL097             :RUECB056.MAREA-CULTR-AGRIC,
POL097             :RUECB056.MAREA-PASTA,
POL097             :RUECB056.MDMAIS-AREA,
POL097             :RUECB056.RTPO-EXPLO-IMOV,
POL097             :RUECB056.CSEQ-PATRM-IMOV :WRK-SEQ-PATRM-NULL
POL097       FROM
POL097              DB2PRD.TIMOV_RURAL_EXPDO
POL097       WHERE
POL097             CCLUB              = :RUECB056.CCLUB            AND
POL097             RIMOV_RURAL        = :RUECB056.RIMOV-RURAL      AND
POL097             HULT_ATULZ_EFETU   < :RUECB056.HULT-ATULZ-EFETU
           END-EXEC.

           IF    ( SQLCODE   NOT  =  ZEROS AND +100 AND -811 )   OR
                 ( SQLWARN0       =    'W'   )
                   MOVE 'DB2'                    TO   ERR-TIPO-ACESSO
POL097             MOVE 'IMOV_RURAL_EXPDO  '     TO   ERR-DBD-TAB
                   MOVE 'SELECT  '               TO   ERR-FUN-COMANDO
                   MOVE SQLCODE                  TO   ERR-SQL-CODE
                   MOVE 0001                     TO   ERR-LOCAL
                   MOVE SPACES                   TO   ERR-SEGM
                   PERFORM   9999-ROTINA-ERRO.

POL097     IF      SQLCODE   EQUAL   ZEROS
POL097     AND     WRK-SEQ-PATRM-NULL  EQUAL  -1
POL097             MOVE ZEROS       TO CSEQ-PATRM-IMOV OF RUECB056.

      *---------------------------------------------------------------*
       11230-99-FIM. EXIT.
      *---------------------------------------------------------------*

      *---------------------------------------------------------------*
       11240-ACESSA-RUECB051   SECTION.
      *---------------------------------------------------------------*

POL097     MOVE    HULT-EFTVC-ALT OF RUECB063
POL097                               TO   HULT-ATULZ-EFETU OF RUECB051
           MOVE   COD-PRODUTO     OF NAVEGACAO-RURAL
           TO     CPRODT-RURAL    OF RUECB051
POL097     MOVE    CCLUB-RUEC        TO   CCLUB    OF RUECB051.

           EXEC SQL
             SELECT
                   CPRODT_RURAL
             INTO
                   :RUECB051.CPRODT-RURAL
             FROM
                    DB2PRD.TCULTR_AGRIC_IMOV
             WHERE
                   CCLUB              = :RUECB051.CCLUB              AND
                   CSEQ_IMOV_RURAL    = :RUECB056.CSEQ-IMOV-RURAL    AND
                   CPRODT_RURAL       = :RUECB051.CPRODT-RURAL       AND
POL097             HULT_ATULZ_EFETU   < :RUECB051.HULT-ATULZ-EFETU
           END-EXEC.

           IF    ( SQLCODE   NOT  =  ZEROS AND +100)   OR
                 ( SQLWARN0       =    'W'   )
                   MOVE 'DB2'                    TO   ERR-TIPO-ACESSO
                   MOVE 'V01CULTR_AGRIC    '     TO   ERR-DBD-TAB
                   MOVE 'SELECT  '               TO   ERR-FUN-COMANDO
                   MOVE SQLCODE                  TO   ERR-SQL-CODE
                   MOVE 0002                     TO   ERR-LOCAL
                   MOVE SPACES                   TO   ERR-SEGM
                   PERFORM   9999-ROTINA-ERRO.

      *---------------------------------------------------------------*
          11240-99-FIM. EXIT.
      *---------------------------------------------------------------*

      *---------------------------------------------------------------*
       11250-ACESSA-RUECB060   SECTION.
      *---------------------------------------------------------------*

POL097     MOVE    HULT-EFTVC-ALT OF RUECB063
POL097                               TO   HULT-ATULZ-EFETU OF RUECB060
           MOVE   COD-PRODUTO     OF  NAVEGACAO-RURAL
           TO     CPRODT-RURAL    OF  RUECB060
POL097     MOVE    CCLUB-RUEC        TO   CCLUB    OF RUECB060.

           EXEC SQL
             SELECT
                   CPRODT_RURAL
             INTO
                   :RUECB060.CPRODT-RURAL
             FROM
                    DB2PRD.TRBNHO_IMOV_RURAL
             WHERE
                   CCLUB              = :RUECB060.CCLUB              AND
                   CSEQ_IMOV_RURAL    = :RUECB056.CSEQ-IMOV-RURAL    AND
                   CPRODT_RURAL       = :RUECB060.CPRODT-RURAL       AND
POL097             HULT_ATULZ_EFETU   = :RUECB060.HULT-ATULZ-EFETU
           END-EXEC.

           IF    ( SQLCODE   NOT  =  ZEROS AND +100)   OR
                 ( SQLWARN0       =    'W'   )
                   MOVE 'DB2'                    TO   ERR-TIPO-ACESSO
                   MOVE 'V01RBNHO_RURAL    '     TO   ERR-DBD-TAB
                   MOVE 'SELECT  '               TO   ERR-FUN-COMANDO
                   MOVE SQLCODE                  TO   ERR-SQL-CODE
                   MOVE 0003                     TO   ERR-LOCAL
                   MOVE SPACES                   TO   ERR-SEGM
                   PERFORM   9999-ROTINA-ERRO.

      *---------------------------------------------------------------*
       11250-99-FIM. EXIT.
      *---------------------------------------------------------------*

      *---------------------------------------------------------------*
POL097 11255-ACESSA-RUECB063   SECTION.
      *---------------------------------------------------------------*

POL097     MOVE    CCLUB-RUEC        TO   CCLUB
POL097                                    OF RUECB063
POL097     MOVE    'V'               TO   CSIT-CAD-CLI OF RUECB063.

POL097     EXEC SQL
POL097       SELECT  HULT_EFTVC_ALT
POL097       INTO  :RUECB063.HULT-EFTVC-ALT
POL097       FROM   DB2PRD.TRSUMO_CLI_RURAL
POL097       WHERE CCLUB              = :RUECB063.CCLUB
POL097         AND CSIT_CAD_CLI       = :RUECB063.CSIT-CAD-CLI
POL097     END-EXEC.

POL097     IF    ( SQLCODE   NOT  =  ZEROS)  OR
POL097           ( SQLWARN0       =    'W'   )
POL097             MOVE 'DB2'                    TO   ERR-TIPO-ACESSO
POL097             MOVE 'TRSUMO_CLI_RURAL  '     TO   ERR-DBD-TAB
POL097             MOVE 'SELECT  '               TO   ERR-FUN-COMANDO
POL097             MOVE SQLCODE                  TO   ERR-SQL-CODE
POL097             MOVE '001A'                   TO   ERR-LOCAL
POL097             MOVE SPACES                   TO   ERR-SEGM
POL097             PERFORM   9999-ROTINA-ERRO.

      *---------------------------------------------------------------*
       11255-99-FIM. EXIT.
      *---------------------------------------------------------------*

      *---------------------------------------------------------------*
POL097 11260-ACESSA-IMOVEL     SECTION.
      *---------------------------------------------------------------*

POL097     MOVE    HULT-EFTVC-ALT OF RUECB063
POL097                               TO   HULT-ATULZ-EFETU OF RUECB055
BSI214     IF  CSEQ-PATRM-IMOV OF RUECB056 NOT LESS ZEROS
POL097         EXEC SQL
POL097           SELECT
POL097                 CMATR_IMOV_REG,
POL097                 CMUN_RURAL
POL097           INTO
POL097                 :RUECB055.CMATR-IMOV-REG ,
POL097                 :RUECB055.CMUN-RURAL
POL097           FROM
POL097                  DB2PRD.TIMOV_RURAL_ARREN
POL097           WHERE
POL097             CCLUB              = :RUECB056.CCLUB              AND
POL097             CSEQ_IMOV_RURAL    = :RUECB056.CSEQ-IMOV-RURAL    AND
POL097             HULT_ATULZ_EFETU   < :RUECB055.HULT-ATULZ-EFETU
POL097         END-EXEC
POL097         IF    ( SQLCODE   NOT  =    ZEROS )   OR
POL097               ( SQLWARN0       =    'W'   )
POL097                 MOVE 'DB2'                  TO   ERR-TIPO-ACESSO
POL097                 MOVE 'TIMOV_RURAL_ARREN '   TO   ERR-DBD-TAB
POL097                 MOVE 'SELECT  '             TO   ERR-FUN-COMANDO
POL097                 MOVE SQLCODE                TO   ERR-SQL-CODE
POL097                 MOVE 0004                   TO   ERR-LOCAL
POL097                 MOVE SPACES                 TO   ERR-SEGM
POL097                 PERFORM   9999-ROTINA-ERRO
POL097         END-IF
POL097         GO  TO  11260-99-FIM
BSI214     END-IF.

POL097     INITIALIZE  R27-AREA-C8430.
POL097     MOVE CCLUB-RUEC              TO R27-ENT-TCLUB-C8430.
POL097     MOVE 09                      TO R27-ENT-CD-ID-TAB-C8430
POL097     MOVE TIPO-PESSOA             TO R27-ENT-TIP-PSSOA-C8430
POL097     MOVE 237                     TO R27-ENT-CEMPRI-C8430
POL097     MOVE 'RUEC'                  TO R27-ENT-CCUSTO-C8430
POL097     MOVE CSEQ-PATRM-IMOV OF RUECB056 TO R27-ENT-COD-SEQ-C8430
pol097     IF  R27-ENT-COD-SEQ-C8430  NOT  EQUAL ZEROS
pol097         COMPUTE R27-ENT-COD-SEQ-C8430 =
pol097                             R27-ENT-COD-SEQ-C8430 - 1
pol097     END-IF

POL097     CALL 'POOL0081'  USING WRK-CADU8430  R27-AREA-C8430.

POL097     IF  R27-SAI-RET-CODE-C8430  EQUAL  03
POL097         MOVE R27-ERRO-AREA-C8430       TO ERRO-AREA
POL097         MOVE R27-SQLCA-C8430           TO SQLCA
POL097         PERFORM   9999-ROTINA-ERRO.

POL097     IF  R27-SAI-RET-CODE-C8430  EQUAL  02
POL097         MOVE    ELE-MENSAG(35)   TO    OUT-MENSAGEM-7000
POL097         MOVE   'ER'         TO   OUT-STATUS-RETORNO-7000
POL097         MOVE    1           TO   WRK-PASSAR-CONTROLE
POL097         PERFORM 14900-FINALIZA-VALIDACAO
POL097         GO  TO  11260-99-FIM.

POL097     IF  R27-SAI-RET-CODE-C8430  EQUAL  04
POL097         MOVE    ELE-MENSAG(36)   TO    OUT-MENSAGEM-7000
POL097         MOVE   'ER'         TO   OUT-STATUS-RETORNO-7000
POL097         MOVE    1           TO   WRK-PASSAR-CONTROLE
POL097         PERFORM 14900-FINALIZA-VALIDACAO
POL097         GO  TO  11260-99-FIM.

POL097     MOVE  R27-TABELA-SAIDA-C8430(01)  TO  CADUL0-CADUB089.
POL097     IF  CCLUB  OF CADUL0-CADUB089 NUMERIC
POL097     AND CCLUB  OF CADUL0-CADUB089 EQUAL CCLUB-RUEC
POL097     AND CSEQ-PATRM-IMOV OF CADUL0-CADUB089  EQUAL
POL097              CSEQ-PATRM-IMOV  OF   RUECB056
POL097         NEXT  SENTENCE
POL097     ELSE
POL097         MOVE    ELE-MENSAG(36)   TO    OUT-MENSAGEM-7000
POL097         MOVE   'ER'         TO   OUT-STATUS-RETORNO-7000
POL097         MOVE    1           TO   WRK-PASSAR-CONTROLE
POL097         PERFORM 14900-FINALIZA-VALIDACAO
POL097         GO  TO  11260-99-FIM.

POL097     MOVE  CMATR-IMOV-REG OF CADUL0-CADUB089  TO
POL097                               CMATR-IMOV-REG OF RUECB055.
POL097     MOVE  CMUN-IMOV      OF CADUL0-CADUB089  TO
POL097                               CMUN-RURAL OF RUECB055.

      *---------------------------------------------------------------*
       11260-99-FIM. EXIT.
      *---------------------------------------------------------------*

      *---------------------------------------------------------------*
       11265-CARREGA-CAMPOS    SECTION.
      *---------------------------------------------------------------*

POL097     MOVE    CMATR-IMOV-REG    OF RUECB055
           TO      CMATR-IMOV        (01)

POL097     MOVE    CSEQ-IMOV-RURAL   OF RUECB056
           TO      CSEQ-IMOV-RURAL   OF NAVEGACAO-RURAL (01)
POL097     MOVE    CMUN-RURAL        OF RUECB055
           TO      CMUN-RURAL        OF NAVEGACAO-RURAL (01)

           ADD     MAREA-CULTR-AGRIC OF RUECB056
                   MAREA-PASTA OF RUECB056
                   MDMAIS-AREA OF RUECB056
                   GIVING                   MAREA-IMOV-RURAL (01).

           MOVE    ZEROS       TO      CINDCD-AREA-PRINC(01).

           MOVE    SPACES      TO      OUT-STATUS-RETORNO-7000.

           PERFORM 14900-FINALIZA-VALIDACAO.

      *---------------------------------------------------------------*
       11265-99-FIM. EXIT.
      *---------------------------------------------------------------*
      *---------------------------------------------------------------*
       11287-ABRE-LISTA        SECTION.
      *---------------------------------------------------------------*

           PERFORM  11288-LIMPA-TELA-RUEC7530  VARYING IND
           FROM     1    BY   1     UNTIL    IND  GREATER  12

           MOVE   CCLUB-RUEC          OF  NAVEGACAO-RURAL
           TO     OUT-CCLUB-T753          INP-CCLUB-T753

           MOVE   TIPO-EXPLOR-RURAL   OF  NAVEGACAO-RURAL
           TO     OUT-EXPLO-T753          INP-EXPLO-T753

           MOVE    ZEROS                TO   WRK-QT-DESPREZAR

           INITIALIZE    DADOS-IMOVEIS

           PERFORM 11290-MONTAR-OCORR.

      *---------------------------------------------------------------*
       11287-99-FIM. EXIT.
      *---------------------------------------------------------------*

      *---------------------------------------------------------------*
       11288-LIMPA-TELA-RUEC7530        SECTION.
      *---------------------------------------------------------------*

           MOVE   ZEROS    TO       7530-CSEQ-IMOV-RURAL  (IND)
                                    7530-CMUN-RURAL       (IND)
                                    7530-MAREA-IMOV-RURAL (IND).

           MOVE   SPACES   TO       7530-DESPROTEGE-SEL   (IND).

      *---------------------------------------------------------------*
       11288-99-FIM. EXIT.
      *---------------------------------------------------------------*

      ******************************************************************
      *     ESSA ROTINA EH   A PRINCIPAL DO PROGRAMA, RESPONSAVEL      *
      *     PELOS ACESSOS A TABELA, PELO POSICIONAMENTO DO CURSOR      *
      *     NA PAGINA DESEJADA  E  PELA MOVIMENTACAO DAS OCORREN-      *
      *     CIAS DO CURSOR PARA A TELA.                                *
      ******************************************************************

      *----------------------------------------------------------------*
       11290-MONTAR-OCORR         SECTION.
      *----------------------------------------------------------------*

POL097     INITIALIZE  R22-AREA-R7722.
POL097     MOVE 'RUEC7530'        TO R22-PGMCHMD-R7722
POL097     MOVE 'P'               TO R22-FUNCAO-R7722
POL097     MOVE  12               TO R22-QPESQ-R7722
POL097     MOVE  WRK-QT-DESPREZAR TO R22-QDESPR-R7722
POL097     MOVE  CCLUB-RUEC       TO R22-CCLUB-R7722
POL097     MOVE  TIPO-PESSOA      TO R22-TPSSOA-ENV-R7722
POL097     MOVE  ZEROS            TO R22-SEQPATRM-ENV-R7722

POL097     CALL 'POOL0081'  USING WRK-RUEC7722
POL097                            R22-AREA-R7722 IO-PCB ALT-PCB.

POL097     IF  R22-RET-CODE-R7722  EQUAL 99
POL097         MOVE R22-SQLCA-R7722      TO SQLCA
POL097         MOVE R22-ERRO-AREA-R7722  TO ERRO-AREA
POL097         PERFORM  9999-ROTINA-ERRO.

POL097     IF  R22-RET-CODE-R7722  EQUAL 01
POL097         MOVE    ELE-MENSAG(33)   TO    OUT-MENSAGEM-7000
POL097         MOVE   'ER'         TO   OUT-STATUS-RETORNO-7000
POL097         MOVE    1           TO   WRK-PASSAR-CONTROLE
POL097         PERFORM 14900-FINALIZA-VALIDACAO
POL097         GO     TO      11290-99-FIM.

POL097     IF  R22-RET-CODE-R7722  EQUAL 02
POL097         MOVE    ELE-MENSAG(20)   TO    OUT-MENSAGEM-7000
POL097         MOVE   'ER'         TO   OUT-STATUS-RETORNO-7000
POL097         MOVE    1           TO   WRK-PASSAR-CONTROLE
POL097         PERFORM 14900-FINALIZA-VALIDACAO
POL097         GO     TO      11290-99-FIM.

POL097     IF  R22-RET-CODE-R7722  EQUAL 05
POL097         MOVE    ELE-MENSAG(34)   TO    OUT-MENSAGEM-7000
POL097         MOVE   'ER'         TO   OUT-STATUS-RETORNO-7000
POL097         MOVE    1           TO   WRK-PASSAR-CONTROLE
POL097         PERFORM 14900-FINALIZA-VALIDACAO
POL097         GO     TO      11290-99-FIM.

POL097     MOVE     1      TO     WRK-TEM-REGISTROS
POL097     MOVE     0      TO     WRK-PASSAR-CONTROLE.
           MOVE   'S'      TO     WRK-PRIM-ATRIB.

POL097     PERFORM  11255-ACESSA-RUECB063.

POL097     PERFORM    VARYING  IND   FROM     1  BY  1
                      UNTIL    IND   GREATER  12    OR
POL097                         IND   GREATER R22-QRETRN-R7722  OR
                               WRK-PASSAR-CONTROLE   EQUAL    1
POL097         PERFORM 11300-VER-CAMPOS
POL097         PERFORM 11315-MONTATELA-GUARDA-VALORES
POL097     END-PERFORM

           IF  WRK-PASSAR-CONTROLE   EQUAL    1
               GO     TO      11290-99-FIM.

      ***** TESTA SE EH FIM DE AMOSTRAGEM OU NAO

POL097     IF  R22-RET-CODE-R7722  EQUAL  03
               MOVE '*'                 TO   OUT-FIMAMOS-T753.

           IF  INP-PAG-T753-R    EQUAL   ZEROS
               MOVE   1   TO   OUT-PAG-T753-R.

      *----regrava ao mostrar a tela para atualizar os dados movidos
      *    para tela-ruec7530, exceto se erro de consistencia ou
      *    pf invalida

           IF  INP-PFK-T753  EQUAL '*'  OR  WRK-ERRO EQUAL '*'
               IF  INP-PFK-T753  EQUAL '*'
                   NEXT SENTENCE
               ELSE
                   IF  WRK-ERRO EQUAL '*'
                       PERFORM  11297-VER-MENSAGEM
                       VARYING  IND   FROM     1  BY  1
                       UNTIL    IND   GREATER  12
                       MOVE  ELE-MENSAG (WRK-MSG)  TO  OUT-MENSAG-T753
                   ELSE
                       NEXT SENTENCE
           ELSE
               IF ( INP-PFK-T753 EQUAL  '.' )  OR
                  ( INP-PFK-T753 EQUAL  'C' ) AND
                  ( WRK-ERRO     EQUAL  ' ')
                   MOVE  ELE-MENSAG (WRK-MSG)  TO  OUT-MENSAG-T753
               ELSE
                   PERFORM  12850-REGRAVAR-RUECB042.

      *----------------------------------------------------------------*
       11290-99-FIM. EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       11297-VER-MENSAGEM           SECTION.
      *----------------------------------------------------------------*

           IF  WRK-MENSAG  (IND)  NOT EQUAL    ZEROS
               MOVE    WRK-MENSAG  (IND)    TO    WRK-MSG
               MOVE    12    TO     IND.

      *----------------------------------------------------------------*
       11297-99-FIM. EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       11300-VER-CAMPOS                 SECTION.
      *----------------------------------------------------------------*

      *    O TRATAMENTO DAS FINALIDADES 12, 13, 14, 15, 17, 19, 20 E 22
      *    E UMA EXCESSAO A REGRA DE VERIFICACAO DE PRODUTO  FINANCIADO
      *    EM IMOVEIS DE PROPONENTE, POIS ESTES PRODUTOS SE  REFEREM  A
      *    SUBSIDIOS DE PRODUCAO.
      *
      *----VERIFICAR-PRODUTO-------------------------------------------
      *----------------------------------------- AGRICULTURA ---------*

POL097     MOVE CCLUB-RUEC              TO CCLUB OF RUECB051
POL097                                     CCLUB OF RUECB060
POL097     MOVE R22-SQIMOV-R7722 (IND)  TO CSEQ-IMOV-RURAL OF RUECB051
POL097                                     CSEQ-IMOV-RURAL OF RUECB060
POL097     MOVE    HULT-EFTVC-ALT OF RUECB063
POL097                               TO   HULT-ATULZ-EFETU OF RUECB051
POL097                                    HULT-ATULZ-EFETU OF RUECB060

           IF  COD-FINALIDADE NOT EQUAL  12 AND 13 AND 14 AND 15 AND 17
BOOG                                AND  19 AND 20 AND 22 AND 60 AND 61
BOOG                                AND  62 AND 63 AND 64
               IF  COD-ATIVIDADE EQUAL 1 OR 3 OR 5
POL097             PERFORM 11300-100-CURSOR-RUECB051
               ELSE
      *----------------------------------------- PECUARIA ------------*
POL097            PERFORM 11300-200-CURSOR-RUECB060.

      *----------------------------------------------------------------*
       11300-99-FIM.     EXIT.
      *----------------------------------------------------------------*

      *---------------------------------------------------------------*
POL097 11300-100-CURSOR-RUECB051   SECTION.
      *---------------------------------------------------------------*

           EXEC SQL
POL097                 OPEN    CV51
           END-EXEC.

           IF    ( SQLCODE   NOT  =    ZEROS )   OR
                 ( SQLWARN0       =    'W'   )
                   MOVE 'DB2'                    TO   ERR-TIPO-ACESSO
POL097             MOVE 'TCULTR_AGRIC'           TO   ERR-DBD-TAB
                   MOVE 'OPEN    '               TO   ERR-FUN-COMANDO
                   MOVE SQLCODE                  TO   ERR-SQL-CODE
                   MOVE 0008                     TO   ERR-LOCAL
                   MOVE SPACES                   TO   ERR-SEGM
                   PERFORM   9999-ROTINA-ERRO.

           MOVE  'N'          TO           WRK-NAO-ACHOU.
           MOVE   999999999   TO           SQLCODE

           PERFORM UNTIL  SQLCODE  =  +100   OR ZEROS

              EXEC SQL
POL097          FETCH    CV51   INTO
POL097             :RUECB051.CPRODT-RURAL
              END-EXEC

              IF    ( SQLCODE   NOT  =  ZEROS  AND  +100 )   OR
                    ( SQLWARN0       =   'W'   )
                    MOVE 'DB2'                    TO   ERR-TIPO-ACESSO
POL097              MOVE 'TCULTR_AGRIC'          TO   ERR-DBD-TAB
                    MOVE 'FETCH '                 TO   ERR-FUN-COMANDO
                    MOVE SQLCODE                  TO   ERR-SQL-CODE
                    MOVE 0009                     TO   ERR-LOCAL
                    MOVE SPACES                   TO   ERR-SEGM
                    PERFORM   9999-ROTINA-ERRO
              END-IF

              IF  SQLCODE  = ZEROS
                  IF  COD-PRODUTO  OF  NAVEGACAO-RURAL  NOT  =
POL097                               CPRODT-RURAL  OF  RUECB051
                     MOVE   999999999    TO     SQLCODE
                  END-IF
              END-IF

           END-PERFORM.

           IF  SQLCODE   =  +100
               MOVE  'S'    TO  WRK-NAO-ACHOU.

           EXEC SQL
POL097       CLOSE   CV51
           END-EXEC.

           IF    ( SQLCODE   NOT  =    ZEROS )   OR
                 ( SQLWARN0       =    'W'   )
                   MOVE 'DB2'                    TO   ERR-TIPO-ACESSO
POL097             MOVE 'TCULTR_AGRIC'           TO   ERR-DBD-TAB
                   MOVE 'CLOSE   '               TO   ERR-FUN-COMANDO
                   MOVE SQLCODE                  TO   ERR-SQL-CODE
                   MOVE 0010                     TO   ERR-LOCAL
                   MOVE SPACES                   TO   ERR-SEGM
                   PERFORM   9999-ROTINA-ERRO.

      *---------------------------------------------------------------*
       11300-100-99-FIM.
      *---------------------------------------------------------------*

      *---------------------------------------------------------------*
POL097 11300-200-CURSOR-RUECB060   SECTION.
      *---------------------------------------------------------------*

           EXEC SQL
POL097                 OPEN    CV60
           END-EXEC.

           IF    ( SQLCODE   NOT  =    ZEROS )   OR
                 ( SQLWARN0       =    'W'   )
                   MOVE 'DB2'                    TO   ERR-TIPO-ACESSO
POL097             MOVE 'TRBNHO_IMOV_RURAL'      TO   ERR-DBD-TAB
                   MOVE 'OPEN    '               TO   ERR-FUN-COMANDO
                   MOVE SQLCODE                  TO   ERR-SQL-CODE
                   MOVE 0011                     TO   ERR-LOCAL
                   MOVE SPACES                   TO   ERR-SEGM
                   PERFORM   9999-ROTINA-ERRO.

           MOVE  'N'     TO           WRK-NAO-ACHOU.

           MOVE   999999999     TO           SQLCODE

           PERFORM UNTIL  SQLCODE  =  +100   OR ZEROS

              EXEC SQL
POL097          FETCH    CV60   INTO
POL097             :RUECB060.CPRODT-RURAL
              END-EXEC

              IF    ( SQLCODE   NOT  =  ZEROS  AND  +100 )   OR
                    ( SQLWARN0       =   'W'   )
                    MOVE 'DB2'                    TO   ERR-TIPO-ACESSO
POL097              MOVE 'TRBNHO_IMOV_RURAL'     TO   ERR-DBD-TAB
                    MOVE 'FETCH '                 TO   ERR-FUN-COMANDO
                    MOVE SQLCODE                  TO   ERR-SQL-CODE
                    MOVE 0012                     TO   ERR-LOCAL
                    MOVE SPACES                   TO   ERR-SEGM
                    PERFORM   9999-ROTINA-ERRO
              END-IF

              IF  SQLCODE  = ZEROS
                  IF  COD-PRODUTO  OF  NAVEGACAO-RURAL  NOT  =
POL097                               CPRODT-RURAL  OF  RUECB060
      *----------------------------------------------------------------
      *-    ESTE PROCEDIMENTO EH EXCLUSIVO PARA  FINALIDADE 6 (CUSTEIO
      *  PECUARIO) ONDE O PRODUTO FINANCIADO NAO BATE COM O PRODUTO DO
      *  IMOVEL.
      *--------------------------------------------------------------
                     IF  COD-FINALIDADE  OF NAVEGACAO-RURAL  EQUAL  6
                         PERFORM  11300-210-ACESSA-RURCV010
                         IF  SQLCODE    =   ZEROS
                             CONTINUE
                         ELSE
                            MOVE   999999999    TO     SQLCODE
                         END-IF
                     ELSE
                           MOVE   999999999     TO     SQLCODE
                     END-IF
                 END-IF
              END-IF

           END-PERFORM.

           IF  SQLCODE   =  +100
               MOVE  'S'    TO  WRK-NAO-ACHOU.

           EXEC SQL
POL097       CLOSE   CV60
           END-EXEC.

           IF    ( SQLCODE   NOT  =    ZEROS )   OR
                 ( SQLWARN0       =    'W'   )
                   MOVE 'DB2'                    TO   ERR-TIPO-ACESSO
POL097             MOVE 'TRBNHO_IMOV_RURAL'      TO   ERR-DBD-TAB
                   MOVE 'CLOSE   '               TO   ERR-FUN-COMANDO
                   MOVE SQLCODE                  TO   ERR-SQL-CODE
                   MOVE 0013                     TO   ERR-LOCAL
                   MOVE SPACES                   TO   ERR-SEGM
                   PERFORM   9999-ROTINA-ERRO.

      *---------------------------------------------------------------*
       11300-200-99-FIM.
      *---------------------------------------------------------------*

      *---------------------------------------------------------------*
       11300-210-ACESSA-RURCV010   SECTION.
      *---------------------------------------------------------------*

           MOVE   16                             TO
                                             CFNALD-RURAL OF  RURCV010
POL097     MOVE   CPRODT-RURAL  OF  RUECB060     TO
                                        CPRODT-RURAL  OF   RURCV010.
           EXEC SQL
             SELECT
                   CFNALD_RURAL,
                   CPRODT_RURAL
             INTO
                   :RURCV010.CFNALD-RURAL,
                   :RURCV010.CPRODT-RURAL
             FROM
                    DB2PRD.V01FINRURA_PRORURA
             WHERE
                   CFNALD_RURAL       = :RURCV010.CFNALD-RURAL   AND
                   CPRODT_RURAL       = :RURCV010.CPRODT-RURAL
           END-EXEC.

           IF    ( SQLCODE   NOT  =    ZEROS AND +100 )   OR
                 ( SQLWARN0       =    'W'   )
                   MOVE 'DB2'                    TO   ERR-TIPO-ACESSO
                   MOVE 'V01FINRURA_PRORURA'     TO   ERR-DBD-TAB
                   MOVE 'SELECT  '               TO   ERR-FUN-COMANDO
                   MOVE SQLCODE                  TO   ERR-SQL-CODE
                   MOVE 0008                     TO   ERR-LOCAL
                   MOVE SPACES                   TO   ERR-SEGM
                   PERFORM   9999-ROTINA-ERRO.

      *---------------------------------------------------------------*
       11300-210-FIM. EXIT.
      *---------------------------------------------------------------*

      *----------------------------------------------------------------*
       11315-MONTATELA-GUARDA-VALORES      SECTION.
      *----------------------------------------------------------------*

POL097     MOVE    R22-RIMOV-R7722 (IND)
           TO      OUT-DENO-T753             (IND)

POL097     MOVE    R22-CMATRIC-R7722 (IND)
           TO      OUT-MATRI-T753            (IND)

POL097     MOVE    R22-RMUNIC-R7722 (IND)
           TO      OUT-MUNI-T753             (IND)

POL097     MOVE    R22-UF-R7722 (IND)
           TO      OUT-UF-T753               (IND)

POL097     MOVE    R22-SQIMOV-R7722 (IND)
           TO      7530-CSEQ-IMOV-RURAL      (IND)

POL097     MOVE    R22-CMUNIC-R7722 (IND)
           TO      7530-CMUN-RURAL           (IND)

POL097     ADD     R22-MAREACULTR-R7722 (IND)
POL097             R22-MAREARBNHO-R7722 (IND)
POL097             R22-MAREAOUTR-R7722 (IND)
                   GIVING  7530-MAREA-IMOV-RURAL (IND).

           MOVE    SPACES
           TO      OUT-STATUS-T753           (IND)

POL097     IF   R22-CSITIMOV-R7722 (IND)  EQUAL  1
BSI083          MOVE    'DADO C/ GARAN'
                TO      OUT-STATUS-T753      (IND).

POL097     IF   R22-CSITIMOV-R7722 (IND)  EQUAL  2
POL097          MOVE    'FINANCIADO   '
POL097          TO      OUT-STATUS-T753      (IND).

POL097     IF   R22-CSITIMOV-R7722 (IND)  EQUAL  3
                MOVE    'DEFINITIVO   '
                TO      OUT-STATUS-T753      (IND).

POL097     IF   R22-CSITIMOV-R7722 (IND)  EQUAL  4
                MOVE    'COMPROMISSADO'
                TO      OUT-STATUS-T753      (IND).

POL097     IF   R22-CSITIMOV-R7722 (IND)  EQUAL  5
                MOVE    'PENHORADO    '
                TO      OUT-STATUS-T753      (IND).

POL097     IF   R22-CINDEXPDO-R7722 (IND)    EQUAL  2
                MOVE    'NAO EXPLORADO'
                TO      OUT-STATUS-T753      (IND)
                MOVE    WRK-225
                TO      OUT-SEL-ATTR-T753    (IND)
                MOVE    3      TO   WRK-PROT (IND)
                MOVE   'N'     TO   7530-DESPROTEGE-SEL (IND)
                GO      TO   11315-99-FIM.

POL097     IF   R22-RTPOEXPL-R7722 (IND)     NOT EQUAL
                TIPO-EXPLOR-RURAL    OF NAVEGACAO-RURAL
POL097          MOVE R22-RTPOEXPL-R7722      (IND)
                TO      OUT-STATUS-T753      (IND)
                MOVE    WRK-225
                TO      OUT-SEL-ATTR-T753    (IND)
                MOVE    3      TO   WRK-PROT (IND)
                MOVE   'N'     TO   7530-DESPROTEGE-SEL (IND)
                GO      TO   11315-99-FIM.

           IF  COD-FINALIDADE  EQUAL  10
               IF  COD-PRODUTO   EQUAL   162   OR  163
POL097             IF  R22-MAREARBNHO-R7722(IND)     NOT EQUAL   ZEROS
                       CONTINUE
                   ELSE
                     MOVE    'FAZ. SEM PASTO'    TO
                                      OUT-STATUS-T753   (IND)
                     MOVE    WRK-225             TO
                                      OUT-SEL-ATTR-T753 (IND)
                     MOVE    3      TO   WRK-PROT (IND)
                     MOVE   'N'     TO   7530-DESPROTEGE-SEL (IND)
                     GO      TO   11315-99-FIM
                   END-IF
               ELSE
                   CONTINUE
               END-IF
           END-IF

           IF  COD-FINALIDADE   NOT   EQUAL  10
               IF  WRK-NAO-ACHOU    EQUAL  'S'
                   MOVE 'PRODUTO DIFERENTE'    TO
                              OUT-STATUS-T753      (IND)
                   MOVE    WRK-225            TO
                             OUT-SEL-ATTR-T753    (IND)
                   MOVE    3      TO   WRK-PROT (IND)
                   MOVE   'N'     TO   7530-DESPROTEGE-SEL (IND)
                   GO      TO   11315-99-FIM.

           MOVE    0     TO     WRK-ACHEI-CSEQ-IMOV

           PERFORM    11320-PROCURA-CSEQ-IMOV
             VARYING  IND-1  FROM   1   BY   1
             UNTIL    IND-1  GREATER   25

           IF  WRK-ACHEI-CSEQ-IMOV    EQUAL   0
               IF  INP-PFK-T753  EQUAL '*'  OR  WRK-ERRO EQUAL '*'
                   MOVE INP-SEL-T753 (IND) TO OUT-SEL-T753 (IND)
               ELSE
                   MOVE  ' '               TO OUT-SEL-T753 (IND).

POL097     IF R22-CINDEXPDO-R7722 (IND)    EQUAL  2
              MOVE    WRK-225
              TO      OUT-SEL-ATTR-T753    (IND)
              MOVE    3      TO   WRK-PROT (IND)
              MOVE   'N'     TO   7530-DESPROTEGE-SEL (IND)
              GO      TO   11315-99-FIM.

           IF INP-PFK-T753  EQUAL '*'  OR  WRK-ERRO EQUAL '*'
              IF  OUT-SEL-ATTR-T753(IND)  EQUAL  TAB-ATRIBUTO(04)
                  GO    TO   11315-99-FIM.

           MOVE   'S'    TO     7530-DESPROTEGE-SEL(IND).
           MOVE   TAB-ATRIBUTO       (09)    TO
                           OUT-SEL-ATTR-T753  (IND).

           IF  WRK-PRIM-ATRIB   EQUAL  'S'
                MOVE   TAB-ATRIBUTO       (07)     TO
                                         OUT-SEL-ATTR-T753  (IND)
                MOVE   'N'     TO     WRK-PRIM-ATRIB.

      *---------------------------------------------------------------*
       11315-99-FIM. EXIT.
      *---------------------------------------------------------------*

      *---------------------------------------------------------------*
       11320-PROCURA-CSEQ-IMOV             SECTION.
      *---------------------------------------------------------------*

POL097     IF   R22-SQIMOV-R7722 (IND)         EQUAL
                CSEQ-IMOV-RURAL OF IMOVEIS(IND-1)
                MOVE       1      TO      WRK-ACHEI-CSEQ-IMOV
                IF  CINDCD-AREA-PRINC     (IND-1)   EQUAL   0
                    MOVE  'P'     TO      OUT-SEL-T753 (IND)
                    MOVE  25      TO      IND-1
                    PERFORM  11325-VER-SEL
                ELSE
                    MOVE  '*'     TO      OUT-SEL-T753 (IND)
                    MOVE  25      TO      IND-1
                    PERFORM  11325-VER-SEL.

      *---------------------------------------------------------------*
       11320-99-FIM. EXIT.
      *---------------------------------------------------------------*

      *---------------------------------------------------------------*
       11325-VER-SEL                       SECTION.
      *---------------------------------------------------------------*
      *---no caso de erro de consistencia ou pf invalida ver o que se
      *   deve mover no campo sel

           IF  INP-PFK-T753  EQUAL '*'  OR  WRK-ERRO EQUAL '*'
               IF  INP-SEL-T753 (IND)    EQUAL   SPACES
                   IF  OUT-SEL-T753 (IND)  NOT EQUAL  SPACES
                       MOVE  ' '     TO      OUT-SEL-T753 (IND)
                   ELSE
                       NEXT SENTENCE
               ELSE
                   IF  INP-SEL-T753 (IND)  EQUAL  OUT-SEL-T753 (IND)
                       NEXT SENTENCE
                   ELSE
                       MOVE INP-SEL-T753 (IND) TO OUT-SEL-T753 (IND).

      *---------------------------------------------------------------*
       11325-99-FIM. EXIT.
      *---------------------------------------------------------------*

      *---------------------------------------------------------------*
       11330-PROTEGER                      SECTION.
      *---------------------------------------------------------------*

           MOVE   TAB-ATRIBUTO       (03)
           TO     OUT-SEL-ATTR-T753  (IND).

      *---------------------------------------------------------------*
       11330-99-FIM. EXIT.
      *---------------------------------------------------------------*


      ******************************************************************
      *     ESSA ROTINA PROCESSA A MENSAGEM VINDA DA TELA RUECT753     *
      ******************************************************************
      *----------------------------------------------------------------*
       12000-PROCESSAR-RUEC7530           SECTION.
      *----------------------------------------------------------------*

           MOVE    WRK-MENSAGEM         TO      INPUT-T753

           MOVE    INP-PROTEG-T753      TO      WRK-TAB-PROTEG

           PERFORM 12840-ACESSA-RUECB042

           IF  INP-PFK-T753 EQUAL  'A'
               MOVE   SPACES  TO     OUT-STATUS-RETORNO-7000
               INITIALIZE   DADOS-IMOVEIS
               PERFORM 12850-REGRAVAR-RUECB042
ROD            MOVE   PROGRAMA-ANT TO     WRK-PGM-CHAMADOR
               PERFORM   10500-PASSAR-CONTROLE
               GO    TO    12000-99-FIM.

           PERFORM    11330-PROTEGER
           VARYING  IND    FROM   1   BY   1
           UNTIL    IND    GREATER   12.

           IF  INP-PFK-T753     EQUAL    '*'
               PERFORM 12100-DEVOLVER-TELA
               MOVE   ELE-MENSAG(4)     TO   OUT-MENSAG-T753
               IF  (WRK-TAB-PROTEG  NOT EQUAL SPACES) AND
                   (WRK-TAB-PROTEG  NOT EQUAL ZEROS)
                   PERFORM  12130-MANTER-ATRIBUTO
                   VARYING  IND  FROM   1   BY   1
                   UNTIL    IND  GREATER   12
               ELSE
                   NEXT SENTENCE
           ELSE
               IF  INP-PFK-T753    EQUAL   '.'    OR    'C'
                   PERFORM  12600-CONSISTE-DADOS-DIGITADOS
                   IF  WRK-ERRO EQUAL '*'
                       PERFORM 12100-DEVOLVER-TELA
                       PERFORM  11297-VER-MENSAGEM
                       VARYING  IND   FROM     1  BY  1
                       UNTIL    IND   GREATER  12
                       MOVE  ELE-MENSAG (WRK-MSG)  TO  OUT-MENSAG-T753
                   ELSE
                   IF WRK-PASSAR-CONTROLE   EQUAL    0
                       PERFORM 12840-ACESSA-RUECB042
                       PERFORM 12100-DEVOLVER-TELA
                   ELSE
                       NEXT SENTENCE
               ELSE
                   PERFORM  12200-DEVOLVER-CAMPOS-FIXOS
                   IF  INP-PFK-T753       EQUAL    '7'
                       PERFORM 12300-TRATAR-PF7
                   ELSE
                       IF  INP-PFK-T753      EQUAL    '8'
                           PERFORM 12400-TRATAR-PF8
                       ELSE
                           IF  INP-PFK-T753    EQUAL    '9'
                               PERFORM 12500-TRATAR-PF9.

           PERFORM  12850-REGRAVAR-RUECB042.

      *----------------------------------------------------------------*
       12000-99-FIM. EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       12100-DEVOLVER-TELA SECTION.
      *----------------------------------------------------------------*

           PERFORM  12200-DEVOLVER-CAMPOS-FIXOS

           COMPUTE WRK-QT-DESPREZAR = (INP-PAG-T753-R - 1) * 12.

           PERFORM 11290-MONTAR-OCORR.

      *----------------------------------------------------------------*
       12100-99-FIM. EXIT.
      *----------------------------------------------------------------*

      *****************************************************************
      *  ESSA ROTINA SERVE PARA MANTER OS ATRIBUTOS DOS CAMPOS QUANDO *
      *  SE PRESSIONA UMA PF INVALIDA                                 *
      *****************************************************************
      *---------------------------------------------------------------*
       12130-MANTER-ATRIBUTO SECTION.
      *---------------------------------------------------------------*

           IF  WRK-PROT (IND)  NOT  EQUAL    SPACES
               MOVE    WRK-PROT(IND)     TO    WRK-ATRIB-R
               MOVE    TAB-ATRIBUTO       (WRK-ATRIB)
               TO      OUT-SEL-ATTR-T753  (IND).

      *---------------------------------------------------------------*
       12130-99-FIM. EXIT.
      *---------------------------------------------------------------*

      *----------------------------------------------------------------*
       12200-DEVOLVER-CAMPOS-FIXOS         SECTION.
      *----------------------------------------------------------------*

           MOVE    INP-PROTEG-T753      TO     OUT-PROTEG-T753
           MOVE    INP-EXPLO-T753       TO     OUT-EXPLO-T753
           MOVE    INP-PAG-T753-R       TO     OUT-PAG-T753-R
           MOVE    INP-TERMINAL-T753    TO     OUT-TERMINAL-T753
           MOVE    INP-FASE-T753        TO     OUT-FASE-T753
           MOVE    INP-CCLUB-T753       TO     OUT-CCLUB-T753.

      *----------------------------------------------------------------*
       12200-99-FIM. EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       12300-TRATAR-PF7 SECTION.
      *----------------------------------------------------------------*

           IF  INP-PAG-T753-R    EQUAL    ZEROS
               PERFORM 12100-DEVOLVER-TELA
               MOVE    ELE-MENSAG(1)    TO   OUT-MENSAG-T753
               GO                       TO   12300-99-FIM.

           IF  INP-PAG-T753-R    EQUAL    1
               PERFORM 12100-DEVOLVER-TELA
               MOVE    ELE-MENSAG(32)   TO   OUT-MENSAG-T753
               GO                       TO   12300-99-FIM.

           COMPUTE WRK-QT-DESPREZAR = (INP-PAG-T753-R - 2) * 12.

           PERFORM 11290-MONTAR-OCORR.

           COMPUTE OUT-PAG-T753-R   = INP-PAG-T753-R - 1.

      *----------------------------------------------------------------*
       12300-99-FIM. EXIT.
      *----------------------------------------------------------------*
           EJECT
      *----------------------------------------------------------------*
       12400-TRATAR-PF8 SECTION.
      *----------------------------------------------------------------*

           IF  INP-PAG-T753-R EQUAL ZEROS
               PERFORM 12100-DEVOLVER-TELA
               MOVE    ELE-MENSAG(1)    TO   OUT-MENSAG-T753
               GO                       TO   12400-99-FIM.

           IF  INP-FIMAMOS-T753 EQUAL '*'
               PERFORM 12100-DEVOLVER-TELA
               MOVE    ELE-MENSAG(31)   TO   OUT-MENSAG-T753
               GO                       TO   12400-99-FIM.

           COMPUTE WRK-QT-DESPREZAR = INP-PAG-T753-R * 12.

           INITIALIZE      TELA-RUEC7530-GRUPO.

           PERFORM  12850-REGRAVAR-RUECB042

           PERFORM 11290-MONTAR-OCORR.

           COMPUTE OUT-PAG-T753-R   = INP-PAG-T753-R + 1.

      *----------------------------------------------------------------*
       12400-99-FIM. EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       12500-TRATAR-PF9 SECTION.
      *----------------------------------------------------------------*

           IF  (INP-POSI-LINHA-T753    LESS     7)    OR
               (INP-POSI-LINHA-T753    GREATER  18)
               PERFORM 12100-DEVOLVER-TELA
               MOVE ELE-MENSAG(30)  TO   OUT-MENSAG-T753
           ELSE
               PERFORM  12505-PF9-MOVE-OCCURS
               VARYING  IND    FROM      1    BY   1
               UNTIL    IND    GREATER   12
               MOVE   INP-EXPLO-T753
               TO     HOLD-7530-TIPO-EXPLOR-RURAL
               MOVE   INP-MENSAG-T753
               TO     HOLD-7530-MSG     OF  HOLD-TELA-RUEC7530
               MOVE   INP-POSI-LINHA-T753
               TO     HOLD-7530-LINHA   OF  HOLD-TELA-RUEC7530
               MOVE   INP-POSI-COLUNA-T753
               TO     HOLD-7530-COLUNA  OF  HOLD-TELA-RUEC7530
               MOVE   INP-PAG-T753-R
               TO     HOLD-7530-PAGINA  OF  HOLD-TELA-RUEC7530
               SUBTRACT  6  FROM  INP-POSI-LINHA-T753
               GIVING   WRK-OCCUR
               MOVE   7530-CSEQ-IMOV-RURAL (WRK-OCCUR)
               TO     HOLD-7530-CSEQ-IMOV-RURAL
               PERFORM  12850-REGRAVAR-RUECB042
               MOVE   'RUEC7580'
               TO     WRK-PGM-CHAMADOR
               PERFORM   10500-PASSAR-CONTROLE.

      *----------------------------------------------------------------*
       12500-99-FIM. EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       12505-PF9-MOVE-OCCURS          SECTION.
      *----------------------------------------------------------------*
           IF  INP-DENO-T753 (IND)   EQUAL     SPACES
               GO    TO     12505-99-FIM.

           MOVE  7530-DESPROTEGE-SEL(IND)
                                    TO  HOLD-7530-DESPROTEGE-SEL(IND).
           MOVE  INP-SEL-T753    (IND)  TO  HOLD-7530-SELECIONA  (IND).
           MOVE  INP-DENO-T753   (IND)  TO  HOLD-7530-DENOMINACAO(IND).
           MOVE  INP-MATRI-T753  (IND)  TO  HOLD-7530-MATRICULA  (IND).
           MOVE  INP-MUNI-T753   (IND)  TO  HOLD-7530-MUNICIPIO  (IND).
           MOVE  INP-UF-T753     (IND)  TO  HOLD-7530-UF         (IND).
           MOVE  INP-STATUS-T753 (IND)  TO  HOLD-7530-STATUS     (IND).

      *----------------------------------------------------------------*
       12505-99-FIM. EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       12600-CONSISTE-DADOS-DIGITADOS       SECTION.
      *----------------------------------------------------------------*

           MOVE    SPACES
           TO      OUT-PROTEG-T753      INP-PROTEG-T753
           INITIALIZE                   WRK-TAB-PROTEG

      *--- caso uma das ocorrencias mostradas na tela exista em imoveis
      *    (ruecb042), inicializar esta ocorrencia em imoveis

           PERFORM  12605-ACHA-CSEQ-EM-IMOVEIS
             VARYING  IND  FROM   1   BY   1
             UNTIL    IND  GREATER   12

           PERFORM  12615-VER-CAMPO-SEL
             VARYING  IND  FROM   1   BY   1
             UNTIL    IND  GREATER   12
             OR       INP-DENO-T753 (IND)   EQUAL   SPACES

           IF  WRK-ERRO  EQUAL   '*'
               MOVE    WRK-TAB-PROTEG
               TO      OUT-PROTEG-T753      INP-PROTEG-T753
               GO    TO     12600-99-FIM.

      *--- verificar se o numero de selecoes nao ultrapassou o limite
      *    da tabela de 25 ocorrencias

           MOVE    ZEROS     TO    WRK-QTSEL
           PERFORM  12617-VER-QTSEL
           VARYING  IND    FROM   1   BY   1
           UNTIL    IND    GREATER   25

           PERFORM  12603-VER-QTSEL-TELA
           VARYING  IND    FROM   1   BY   1
           UNTIL    IND    GREATER   12

           IF  WRK-QTSEL       GREATER     25
               MOVE    25                 TO   WRK-MENSAG(IND)
               MOVE    '*'                TO   WRK-ERRO
               MOVE    WRK-TAB-PROTEG
               TO      OUT-PROTEG-T753      INP-PROTEG-T753
               GO      TO     12600-99-FIM
           ELSE
           IF  WRK-QTSEL       EQUAL      ZEROS
               PERFORM  12850-REGRAVAR-RUECB042
               MOVE    29     TO  WRK-MSG
               GO      TO     12600-99-FIM.

      *--- o que foi selecionado mover para a primeira ocorrencia de
      *    cseq-imov-rural zerado

           PERFORM  12620-VER-IMOVEIS-SEL
             VARYING  IND  FROM   1   BY   1
             UNTIL    IND  GREATER   12
             OR       INP-DENO-T753 (IND)   EQUAL   SPACES

           PERFORM  12630-SORTEAR-TABELA-IMOVEIS

      *----SE ENTER ---------------------------------------------------*
           IF  INP-PFK-T753 EQUAL  '.'
               PERFORM  12850-REGRAVAR-RUECB042
               MOVE    27                 TO   WRK-MSG
STEFAB         IF  WRK-ERRO               EQUAL 'I'
.                  MOVE    40             TO   WRK-MSG
STEFAB         END-IF
      *----SE PF11 ----------------------------------------------------*
           ELSE
              IF  INP-PFK-T753 EQUAL  'C'
                  IF  CSEQ-IMOV-RURAL      OF NAVEGACAO-RURAL (01)
                                                  EQUAL       ZEROS
                      MOVE ELE-MENSAG(28)  TO   OUT-MENSAG-T753
*ALTE*                MOVE 28              TO   WRK-MSG
                      MOVE   '1'           TO   WRK-ERRO
                  ELSE
      *----------ver se todos cindcd-area-princ sao diferentes de zero-*
                      PERFORM  12650-VER-QTOS-IMOVEL-PRINC
                      VARYING  IND    FROM   1   BY   1
                      UNTIL    IND    GREATER   25
                      IF  WRK-QTDE-IMOV-PRINC   EQUAL   ZEROS
                          MOVE ELE-MENSAG(29)  TO   OUT-MENSAG-T753
      *                   MOVE   '1'           TO   WRK-ERRO
CLODI                     MOVE   '*'           TO   WRK-ERRO
*ALTE**                   MOVE 29              TO   WRK-MSG
CLODI                     MOVE 29              TO   WRK-MENSAG(01)
                          PERFORM 12601-DEVOLVE-SEL VARYING IND FROM 1
                          BY 1 UNTIL IND  GREATER 12
                          PERFORM  12850-REGRAVAR-RUECB042
                      ELSE
                          MOVE   '  '    TO   OUT-STATUS-RETORNO-7000
                          PERFORM 14900-FINALIZA-VALIDACAO.

      *----------------------------------------------------------------*
       12600-99-FIM. EXIT.
      *----------------------------------------------------------------*

      *---------------------------------------------------------------*
       12601-DEVOLVE-SEL                SECTION.
      *---------------------------------------------------------------*

           IF  INP-SEL-T753 (IND)   EQUAL  '*'
               MOVE  INP-SEL-T753 (IND) TO   OUT-SEL-T753(IND).

      *---------------------------------------------------------------*
       12603-99-FIM. EXIT.
      *---------------------------------------------------------------*
      *---------------------------------------------------------------*
       12603-VER-QTSEL-TELA             SECTION.
      *---------------------------------------------------------------*

           IF  INP-SEL-T753 (IND)  EQUAL  'P'  OR  '*'
               ADD       1       TO      WRK-QTSEL.

      *---------------------------------------------------------------*
       12603-99-FIM. EXIT.
      *---------------------------------------------------------------*

      *---------------------------------------------------------------*
       12605-ACHA-CSEQ-EM-IMOVEIS       SECTION.
      *---------------------------------------------------------------*

           IF   7530-CSEQ-IMOV-RURAL (IND)  NOT EQUAL   ZEROS
                PERFORM  12610-VER-SE-EXISTE-IMOVEIS
                VARYING  IND-1  FROM   1   BY   1
                UNTIL    IND-1  GREATER   25.

      *---------------------------------------------------------------*
       12605-99-FIM. EXIT.
      *---------------------------------------------------------------*

      *---------------------------------------------------------------*
       12610-VER-SE-EXISTE-IMOVEIS      SECTION.
      *---------------------------------------------------------------*

           IF   7530-CSEQ-IMOV-RURAL      (IND)        EQUAL
                CSEQ-IMOV-RURAL         OF NAVEGACAO-RURAL (IND-1)
                INITIALIZE   IMOVEIS      (IND-1)
                MOVE  25      TO      IND-1.

      *---------------------------------------------------------------*
       12610-99-FIM. EXIT.
      *---------------------------------------------------------------*

      *---------------------------------------------------------------*
       12615-VER-CAMPO-SEL              SECTION.
      *---------------------------------------------------------------*

           IF  INP-SEL-T753 (IND)   EQUAL    SPACES OR LOW-VALUES
               GO   TO   12615-99-FIM.

           IF  INP-SEL-T753 (IND)     NOT EQUAL   'P'  AND  '*'
               MOVE    26                 TO   WRK-MENSAG(IND)
               MOVE    TAB-ATRIBUTO (04)  TO   OUT-SEL-ATTR-T753(IND)
               MOVE    4                  TO   WRK-PROT (IND)
               MOVE    '*'                TO   WRK-ERRO
               GO   TO   12615-99-FIM.


EG0617*--- NAO PERMITIR SELECAO DE IMOVEIS SEM IDENTIFICACAO
EG0617     IF  7530-CSEQ-IMOV-RURAL (IND) NOT NUMERIC
EG0617     OR  7530-CSEQ-IMOV-RURAL (IND) EQUAL ZEROS
EG0617         MOVE    37                 TO   WRK-MENSAG(IND)
EG0617         MOVE    TAB-ATRIBUTO (04)  TO   OUT-SEL-ATTR-T753(IND)
EG0617         MOVE   '4'                 TO   WRK-PROT (IND)
EG0617         MOVE    '*'                TO   WRK-ERRO
EG0617         GO   TO   12615-99-FIM
EG0617     END-IF.
EG0617
EG0617     MOVE INP-CCLUB-T753            TO   CCLUB
EG0617                                    OF   RUECB056
EG0617     MOVE ZEROS                     TO   CSEQ-ATULZ-IMOV
EG0617                                    OF   RUECB056
EG0617     MOVE 7530-CSEQ-IMOV-RURAL(IND) TO   CSEQ-IMOV-RURAL
EG0617                                    OF   RUECB056.
EG0617
EG0617     PERFORM 1600-ACESSAR-CAD-IMOVEL.
EG0617
CAR        PERFORM 1700-OBTER-EMPREENDIMENTO.
CAR
CAR
CAR        IF  ( COD-TIPO-BENEFIC       EQUAL  01 OR 03 OR 06
CAR                                        OR  13 OR 16 )
CAR        AND ((RUEC58-EMPREEND(1:2)   EQUAL '22')
CAR        OR   (RUEC58-EMPREEND(1:4)   EQUAL '1201' OR '1211'
CAR                                        OR '1311' OR '1312')
CAR        OR   (RUEC58-EMPREEND         EQUAL '13130360000002'
CAR                                         OR '13130880110002'
CAR                                         OR '13132740000002'
CAR                                         OR '13133160172002'
CAR                                         OR '13133200006002'
CAR                                         OR '13133340000002'
CAR                                         OR '13134020109002'
CAR                                         OR '13136050352042'
CAR                                         OR '13136240217002'
CAR                                         OR '13137705000408'))
CAR            IF  CREG-AMBTL-RURAL    OF RUECB056 EQUAL SPACES
CAR                MOVE 39             TO WRK-MENSAG(IND)
CAR                MOVE TAB-ATRIBUTO (04) TO OUT-SEL-ATTR-T753(IND)
CAR                MOVE '4'            TO WRK-PROT (IND)
CAR                MOVE  '*'           TO WRK-ERRO
CAR                GO TO 12615-99-FIM
CAR            END-IF
CAR        END-IF.
CAR
EG0617     IF  CNIRF-IMOV-RURAL OF RUECB056 EQUAL SPACES
EG0617     AND CCCIR            OF RUECB056 EQUAL SPACES
EG0617     AND CREG-AMBTL-RURAL OF RUECB056 EQUAL SPACES
EG0617         MOVE    38                 TO   WRK-MENSAG(IND)
EG0617         MOVE    TAB-ATRIBUTO (04)  TO   OUT-SEL-ATTR-T753(IND)
EG0617         MOVE   '4'                 TO   WRK-PROT (IND)
EG0617         MOVE    '*'                TO   WRK-ERRO
EG0617         GO   TO   12615-99-FIM
EG0617     END-IF.
EG0617
STEFAB     IF  CNIRF-IMOV-RURAL OF RUECB056 EQUAL SPACES
.          OR  CCCIR            OF RUECB056 EQUAL SPACES
.              MOVE    40                 TO   WRK-MENSAG(IND)
.              MOVE    TAB-ATRIBUTO (04)  TO   OUT-SEL-ATTR-T753(IND)
.              MOVE   '4'                 TO   WRK-PROT (IND)
STEFAB         MOVE    'I'                TO   WRK-ERRO
180825         MOVE    'S'                TO   IND-TEM-IMOVEL
220725         IF CNIRF-IMOV-RURAL OF RUECB056 EQUAL SPACES
180825            MOVE 'N'                TO   IND-TEM-NIRF
''             ELSE
180725            MOVE 'S'                TO   IND-TEM-NIRF
''             END-IF
''             IF CCCIR            OF RUECB056 EQUAL SPACES
180825            MOVE 'N'                TO   IND-TEM-CCIR
''             ELSE
180825            MOVE 'S'                TO   IND-TEM-CCIR
220725         END-IF
.              GO   TO   12615-99-FIM
220725     ELSE
180825         MOVE 'S'                   TO   IND-TEM-NIRF
180825                                         IND-TEM-CCIR
180825                                         IND-TEM-IMOVEL
.          END-IF.
STEFAB
      *--- nao permitir mais de uma ocorrencia de cindcd-area-princ = 0

           IF  INP-SEL-T753 (IND)  EQUAL  'P'
      *--------ver quantos 'P' existem na tela -----------------------*
               MOVE     ZEROS     TO    WRK-QTPRINC
               PERFORM  12618-VER-QTPRINC-NA-TELA
               VARYING  IND-1  FROM   1   BY   1
               UNTIL    IND-1  GREATER   12
               IF  WRK-QTPRINC    GREATER   1
                   MOVE  24                TO  WRK-MENSAG(IND)
                   PERFORM   12616-BRILHA-SEL
                   VARYING  IND-1  FROM   1   BY   1
                   UNTIL    IND-1  GREATER    12
                   MOVE  '*'               TO  WRK-ERRO
                   GO   TO   12615-99-FIM
               ELSE
      *--------ver quantos 'P' existem na tabela imoveis--------------*
                   MOVE     ZEROS     TO    WRK-QTPRINC
                   PERFORM  12619-VER-QTPRINC
                   VARYING  IND-1  FROM   1   BY   1
                   UNTIL    IND-1  GREATER   25
                   IF  WRK-QTPRINC    EQUAL    1
                       MOVE  24
                       TO    WRK-MENSAG(IND)
                       MOVE  TAB-ATRIBUTO (04)
                       TO    OUT-SEL-ATTR-T753(IND)
                       MOVE  4               TO   WRK-PROT (IND)
                       MOVE  '*'             TO   WRK-ERRO.

      *---------------------------------------------------------------*
       12615-99-FIM. EXIT.
      *---------------------------------------------------------------*

      *---------------------------------------------------------------*
       12616-BRILHA-SEL                 SECTION.
      *---------------------------------------------------------------*

           IF  INP-SEL-T753 (IND-1)  EQUAL  'P'
               MOVE  TAB-ATRIBUTO (04) TO  OUT-SEL-ATTR-T753(IND-1)
               MOVE  4                 TO  WRK-PROT (IND-1).

      *---------------------------------------------------------------*
       12616-99-FIM. EXIT.
      *---------------------------------------------------------------*

      *---------------------------------------------------------------*
       12617-VER-QTSEL               SECTION.
      *---------------------------------------------------------------*

           IF   CSEQ-IMOV-RURAL  OF  NAVEGACAO-RURAL (IND)
                                             NOT  EQUAL    ZEROS
                ADD       1       TO      WRK-QTSEL.

      *---------------------------------------------------------------*
       12617-99-FIM. EXIT.
      *---------------------------------------------------------------*

      *---------------------------------------------------------------*
       12618-VER-QTPRINC-NA-TELA        SECTION.
      *---------------------------------------------------------------*

           IF  INP-SEL-T753 (IND-1)  EQUAL  'P'
               ADD       1       TO      WRK-QTPRINC.

      *---------------------------------------------------------------*
       12618-99-FIM. EXIT.
      *---------------------------------------------------------------*

      *---------------------------------------------------------------*
       12619-VER-QTPRINC             SECTION.
      *---------------------------------------------------------------*

           IF    (CSEQ-IMOV-RURAL  OF  NAVEGACAO-RURAL (IND-1)
                 NOT  EQUAL    ZEROS)
                 AND
                 (CINDCD-AREA-PRINC  (IND-1)    EQUAL    ZEROS)
                 MOVE      1       TO      WRK-QTPRINC
*ALTE**          MOVE      0       TO      WRK-QTPRINC
                 MOVE     25       TO      IND-1.

      *---------------------------------------------------------------*
       12619-99-FIM. EXIT.
      *---------------------------------------------------------------*

      *---------------------------------------------------------------*
       12620-VER-IMOVEIS-SEL            SECTION.
      *---------------------------------------------------------------*

           IF  INP-SEL-T753 (IND)    NOT  EQUAL     SPACES
               PERFORM  12625-MOVER-PARA-IMOVEIS
               VARYING  IND-1  FROM   1   BY   1
               UNTIL    IND-1  GREATER   25.

      *---------------------------------------------------------------*
       12620-99-FIM. EXIT.
      *---------------------------------------------------------------*

      *---------------------------------------------------------------*
       12625-MOVER-PARA-IMOVEIS      SECTION.
      *---------------------------------------------------------------*

           IF   CSEQ-IMOV-RURAL         OF NAVEGACAO-RURAL (IND-1)
                                                     EQUAL    ZEROS
                MOVE    7530-CSEQ-IMOV-RURAL      (IND)
                TO      CSEQ-IMOV-RURAL
                                        OF NAVEGACAO-RURAL (IND-1)
                MOVE    INP-MATRI-T753            (IND)
                TO      CMATR-IMOV
                                        OF NAVEGACAO-RURAL (IND-1)
                MOVE    INP-DENO-T753             (IND)
                TO      IIMOV-RURAL
                                        OF NAVEGACAO-RURAL (IND-1)
                MOVE    7530-CMUN-RURAL           (IND)
                TO      CMUN-RURAL
                                        OF NAVEGACAO-RURAL (IND-1)
                MOVE    7530-MAREA-IMOV-RURAL     (IND)
                TO      MAREA-IMOV-RURAL
                                        OF NAVEGACAO-RURAL (IND-1)
                IF  INP-SEL-T753 (IND)  EQUAL  'P'
                    MOVE    0
                    TO      CINDCD-AREA-PRINC     (IND-1)
                    MOVE  25      TO      IND-1
                ELSE
                    MOVE    1
                    TO      CINDCD-AREA-PRINC     (IND-1)
                    MOVE  25      TO      IND-1.

      *---------------------------------------------------------------*
       12625-99-FIM. EXIT.
      *---------------------------------------------------------------*


      *---------------------------------------------------------------*
       12630-SORTEAR-TABELA-IMOVEIS        SECTION.
      *---------------------------------------------------------------*

           INITIALIZE              WRK-TAB-IMOV-ORD.

           MOVE   ZEROS       TO         WRK-ULTIMO-CSEQ

           PERFORM   12635-ORDENA-TAB
                     VARYING  IND-SORT2       FROM      25   BY -1
                     UNTIL    IND-SORT2       EQUAL     0

           MOVE      1      TO     IND-SORT2
           PERFORM   12645-CARREGA-TABELA-ORDENADA
                     VARYING  IND-SORT        FROM      1    BY  1
                     UNTIL    IND-SORT        GREATER   25.

      *---------------------------------------------------------------*
       12630-99-FIM. EXIT.
      *---------------------------------------------------------------*

      *---------------------------------------------------------------*
       12635-ORDENA-TAB                SECTION.
      *---------------------------------------------------------------*

           PERFORM   12640-VER-ULTIMO-CSEQ
                     VARYING  IND-SORT        FROM      1    BY  1
                     UNTIL    IND-SORT        GREATER   25

           MOVE    IMOVEIS      (IND-SORT-SALVO)
           TO      WRK-IMOVEIS  (IND-SORT2)

           MOVE    ZEROS   TO     IMOVEIS      (IND-SORT-SALVO)
           WRK-ULTIMO-CSEQ
           CMUN-RURAL       OF NAVEGACAO-RURAL (IND-SORT-SALVO)
           CSEQ-IMOV-RURAL  OF NAVEGACAO-RURAL (IND-SORT-SALVO)
           MAREA-IMOV-RURAL OF NAVEGACAO-RURAL (IND-SORT-SALVO).

      *---------------------------------------------------------------*
       12635-99-FIM. EXIT.
      *---------------------------------------------------------------*

      *---------------------------------------------------------------*
       12640-VER-ULTIMO-CSEQ           SECTION.
      *---------------------------------------------------------------*

           IF  CSEQ-IMOV-RURAL  OF NAVEGACAO-RURAL (IND-SORT)
                                              GREATER   WRK-ULTIMO-CSEQ
               MOVE   CSEQ-IMOV-RURAL OF NAVEGACAO-RURAL (IND-SORT)
                                                   TO   WRK-ULTIMO-CSEQ
               MOVE   IND-SORT                     TO   IND-SORT-SALVO.

      *---------------------------------------------------------------*
       12640-99-FIM. EXIT.
      *---------------------------------------------------------------*

      *---------------------------------------------------------------*
       12645-CARREGA-TABELA-ORDENADA       SECTION.
      *---------------------------------------------------------------*

           IF  WRK-CSEQ-IMOV-RURAL(IND-SORT) NOT  EQUAL   ZEROS
              MOVE    WRK-IMOVEIS (IND-SORT)  TO  IMOVEIS (IND-SORT2)
              ADD   1    TO  IND-SORT2.

      *---------------------------------------------------------------*
       12645-99-FIM. EXIT.
      *---------------------------------------------------------------*

      *---------------------------------------------------------------*
       12650-VER-QTOS-IMOVEL-PRINC            SECTION.
      *---------------------------------------------------------------*

           IF  CSEQ-IMOV-RURAL OF NAVEGACAO-RURAL (IND)
                                     NOT   EQUAL         ZEROS
               IF  CINDCD-AREA-PRINC (IND)  EQUAL     ZEROS
                   ADD    1        TO      WRK-QTDE-IMOV-PRINC.

      *---------------------------------------------------------------*
       12650-99-FIM. EXIT.
      *---------------------------------------------------------------*


      *----------------------------------------------------------------*
       12840-ACESSA-RUECB042            SECTION.
      *----------------------------------------------------------------*

           MOVE    INP-TERMINAL-T753
           TO      CTERM   OF  RUECB042

           EXEC SQL
               SELECT
                       WNAVG_CREDT_RURAL
               INTO
                       :RUECB042.WNAVG-CREDT-RURAL
               FROM
                       DB2PRD.NAVEGACAO_RURAL
               WHERE
                       CTERM    =   :RUECB042.CTERM
           END-EXEC.

           IF  ( SQLCODE   NOT  =    ZEROS )   OR
               ( SQLWARN0       =    'W'   )
               MOVE    'DB2'                   TO   ERR-TIPO-ACESSO
               MOVE    'NAVEGACAO_RURAL'       TO   ERR-DBD-TAB
               MOVE    'SELECT'                TO   ERR-FUN-COMANDO
               MOVE    SQLCODE                 TO   ERR-SQL-CODE
               MOVE    '0010'                  TO   ERR-LOCAL
               MOVE    SPACES                  TO   ERR-SEGM
               PERFORM 9999-ROTINA-ERRO.

           MOVE    WNAVG-CREDT-RURAL-TEXT   OF  RUECB042
           TO      NAVEG-RURAL.

      *----------------------------------------------------------------*
       12840-99-FIM. EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       12850-REGRAVAR-RUECB042     SECTION.
      *----------------------------------------------------------------*

           MOVE    INP-TERMINAL-T753
           TO      CTERM   OF  RUECB042

STEFAB     IF  WRK-ERRO            EQUAL 'I'
.              MOVE 'S'            TO IND-NIRF
STEFAB     END-IF.

           MOVE    NAVEG-RURAL
           TO      WNAVG-CREDT-RURAL-TEXT   OF  RUECB042
           MOVE    4000
           TO      WNAVG-CREDT-RURAL-LEN    OF  RUECB042

           EXEC SQL
               UPDATE   DB2PRD.NAVEGACAO_RURAL
                  SET  WNAVG_CREDT_RURAL = :RUECB042.WNAVG-CREDT-RURAL
               WHERE
                  CTERM    =   :RUECB042.CTERM
           END-EXEC.

           IF  ( SQLCODE   NOT  =    ZEROS )   OR
               ( SQLWARN0       =    'W'   )
               MOVE    'DB2'                   TO   ERR-TIPO-ACESSO
               MOVE    'NAVEGACAO_RURAL'       TO   ERR-DBD-TAB
               MOVE    'UPDATE'                TO   ERR-FUN-COMANDO
               MOVE    SQLCODE                 TO   ERR-SQL-CODE
               MOVE    '0011'                  TO   ERR-LOCAL
               MOVE    SPACES                  TO   ERR-SEGM
               PERFORM 9999-ROTINA-ERRO.

      *----------------------------------------------------------------*
       12850-99-FIM. EXIT.
      *----------------------------------------------------------------*

      *---------------------------------------------------------------*
       14900-FINALIZA-VALIDACAO        SECTION.
      *---------------------------------------------------------------*

ROD        MOVE   PROGRAMA-ANT
           TO     WRK-PGM-CHAMADOR

           IF  OUT-STATUS-RETORNO-7000    EQUAL   SPACES
               MOVE    'RUEC7530'      TO   PROGRAMA-ANT
               PERFORM 12850-REGRAVAR-RUECB042.

           PERFORM   10500-PASSAR-CONTROLE.
      *----------------------------------------------------------------*
       14900-99-FIM. EXIT.
      *----------------------------------------------------------------*

      *---------------------------------------------------------------*
       15000-POOL0660-RUECT753 SECTION.
      *---------------------------------------------------------------*

           CALL    'POOL0660'          USING   OUTPUT-T753
                                               660-RUECT753.

           IF  RETURN-CODE  NOT EQUAL  ZEROS
               MOVE    'APL'               TO   ERR-TIPO-ACESSO
               MOVE    'POOL0660'          TO   WRK-POOL
               MOVE    RETURN-CODE         TO   WRK-RETURN-CODE
               MOVE    2                   TO   WRK-LOCAL-ERRO
               MOVE    WRK-ERRO-POOL0660   TO   ERR-TEXTO
               PERFORM 9999-ROTINA-ERRO.

           MOVE    WRK-ISRT                TO   WRK-FUNCAO.
           MOVE    OUTPUT-T753             TO   WRK-MENSAGEM.

      *---------------------------------------------------------------*
       15000-99-FIM. EXIT.
      *---------------------------------------------------------------*

EG0617*---------------------------------------------------------------*
EG0617 1600-ACESSAR-CAD-IMOVEL         SECTION.
EG0617*---------------------------------------------------------------*
EG0617
EG0617     EXEC SQL
EG0617       SELECT VALUE ( CNIRF_IMOV_RURAL , ' ' )
EG0617       ,      VALUE ( CCCIR            , ' ' )
EG0617       ,      VALUE ( CREG_AMBTL_RURAL , ' ' )
EG0617       INTO   :RUECB056.CNIRF-IMOV-RURAL
EG0617       ,      :RUECB056.CCCIR
EG0617       ,      :RUECB056.CREG-AMBTL-RURAL
EG0617       FROM   DB2PRD.TIMOV_RURAL_EXPDO
EG0617       WHERE  CCLUB               = :RUECB056.CCLUB
EG0617       AND    CSEQ_IMOV_RURAL     = :RUECB056.CSEQ-IMOV-RURAL
EG0617       AND    CSEQ_ATULZ_IMOV     = :RUECB056.CSEQ-ATULZ-IMOV
EG0617     END-EXEC.
EG0617
EG0617     IF (SQLCODE                 NOT EQUAL ZEROS AND + 100)
EG0617     OR  SQLWARN0                EQUAL 'W'
EG0617         MOVE 'DB2'              TO ERR-TIPO-ACESSO
EG0617         MOVE 'TIMOV_RURAL_EXPDO' TO ERR-DBD-TAB
EG0617         MOVE 'SELECT'           TO   ERR-FUN-COMANDO
EG0617         MOVE SQLCODE            TO   ERR-SQL-CODE
EG0617         MOVE '1600'             TO   ERR-LOCAL
EG0617         MOVE SPACES             TO   ERR-SEGM
EG0617         PERFORM 9999-ROTINA-ERRO
EG0617     END-IF.
EG0617
EG0617     IF  SQLCODE                 EQUAL +100
EG0617         MOVE SPACES             TO CNIRF-IMOV-RURAL OF RUECB056
EG0617                                    CCCIR            OF RUECB056
EG0617                                    CREG-AMBTL-RURAL OF RUECB056
EG0617     END-IF.
EG0617
EG0617*---------------------------------------------------------------*
EG0617 1600-99-FIM.                    EXIT.
EG0617*---------------------------------------------------------------*
EG0617
CAR   *---------------------------------------------------------------*
CAR    1700-OBTER-EMPREENDIMENTO       SECTION.
CAR   *---------------------------------------------------------------*
CAR
CAR        INITIALIZE  RUEC58-AREA.
CAR        MOVE 'RUEC7000'             TO  RUEC58-PGM-CHAMADOR
CAR        MOVE AGENCIA                TO  RUEC58-AGENCIA
CAR        MOVE CCDULA OF AREA-RUEC7000 TO  RUEC58-CEDULA
CAR        MOVE ZEROS                  TO  RUEC58-CONTRATO
CAR        MOVE COD-FINALIDADE         TO  RUEC58-FINALIDADE
CAR        MOVE COD-ATIVIDADE          TO  RUEC58-ATIVIDADE
CAR        MOVE COD-MODALIDADE         TO  RUEC58-MODALIDADE
CAR        MOVE COD-PRODUTO            TO  RUEC58-PRODUTO
CAR        MOVE COD-VARIEDADE          TO  RUEC58-VARIEDADE
CAR        MOVE COD-CESTA-SAFRA        TO  RUEC58-CESTA-SAFRA
CAR        MOVE COD-ZONEAMENTO         TO  WRK-ALPFA-1
CAR        MOVE WRK-NUMERICO-1         TO  RUEC58-ZONEAMENTO
CAR        MOVE COD-CONSORCIO          TO  WRK-ALPFA-1
CAR        MOVE WRK-NUMERICO-1         TO  RUEC58-CONSORCIO
CAR        MOVE COD-TIPO-AGROPEC       TO  RUEC58-TIPO-AGROP
CAR        MOVE COD-TIPO-INTEGR        TO  RUEC58-TIPO-INTEG
CAR        MOVE COD-GRAO-SEMTE         TO  RUEC58-GRAO-SEMENTE
CAR        MOVE COD-TIPO-IRRIG         TO  RUEC58-TIPO-IRRIG
CAR        MOVE COD-TIPO-CULTIVO       TO  RUEC58-TIPO-CULTIVO
CAR        MOVE COD-CICLO-PROD         TO  RUEC58-CICLO-PROD
PRI499     MOVE COD-CLIMA-PROD         TO  RUEC58-CLIMA-PROD
CAR
CAR        EXEC SQL
CAR         SET :WRK-DATA-CEDULA       =   CURRENT_DATE
CAR        END-EXEC.
CAR
CAR        MOVE WRK-DATA-CEDULA        TO  RUEC58-DCDULA
CAR
CAR        CALL  WRK-RUEC8800          USING RUEC58-AREA
CAR                                          IO-PCB
CAR                                          ALT-PCB.
CAR
CAR        IF  RUEC58-COD-RET          EQUAL   99
CAR            MOVE RUEC58-SQLCA       TO SQLCA
CAR            MOVE RUEC58-ERRO-AREA   TO ERRO-AREA
CAR            MOVE 'DB2'              TO ERR-TIPO-ACESSO
CAR            MOVE SPACES             TO ERR-SEGM
CAR            MOVE ERR-PGM            TO ERR-MODULO
CAR            PERFORM 9999-ROTINA-ERRO
CAR        ELSE
CAR            IF  RUEC58-COD-RET      GREATER 0
CAR                MOVE SPACES         TO RUEC58-EMPREEND
CAR            END-IF
CAR        END-IF.
CAR
CAR   *---------------------------------------------------------------*
CAR    1700-99-FIM.                    EXIT.
CAR   *---------------------------------------------------------------*
CAR
      *---------------------------------------------------------------*
       9999-ROTINA-ERRO SECTION.
      *---------------------------------------------------------------*

           MOVE    'SENHAS02'              TO   ERR-PGM.
           MOVE    WRK-COD-USER            TO   ERR-COD-USER.
           MOVE    WRK-COD-DEPTO           TO   ERR-COD-DEPTO.
           MOVE    'RUEC7530'              TO   ERR-MODULO.

           CALL    'POOL7100'           USING   IO-PCB
                                                ERRO-AREA
                                                ALT-PCB
                                                SQLCA.
           GOBACK.

      *---------------------------------------------------------------*
       9999-99-FIM. EXIT.
      *---------------------------------------------------------------*
