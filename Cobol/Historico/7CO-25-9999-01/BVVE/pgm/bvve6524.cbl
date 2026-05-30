      *================================================================*
       IDENTIFICATION                  DIVISION.
      *================================================================*
       PROGRAM-ID. BVVE6524.
       AUTHOR.     FABRICIO MASAKIYO MANCINI
      *================================================================*
      *                   7COMM   -   I N F O R M A T I C A            *
      *----------------------------------------------------------------*
      *    SISTEMA.....: BVVE - TRATAMENTO MENSAGENS NAO FINANCEIRAS   *
      *    PROGRAMA....: BVVE6524                                      *
      *    PROGRAMADOR.: FABRICIO MASAKIYO MANCINI                     *
      *    ANALISTA....: FABRICIO MASAKIYO MANCINI                     *
      *    DATA........: 11/09/2020                                    *
      *----------------------------------------------------------------*
      *    OBJETIVO.:  EXIBIR QUANTIDADE TOTAIS DE ARQUIVOS PROCESSADOS*
      *                AGRUPADAS POR INTERVALOS DE TEMPO, POR DATA,    *
      *                STATUS, FASE(QUANDO SE TRARAR DE STAUS DE ERRO) *
      *                E GRUPO DE SERVICO. OS TOTAIS SAO POR ARQUIVOS  *
      *                ENVIADOS, RECEBIDAS E GERAL.                    *
      *----------------------------------------------------------------*
      *    OBSERVACOES:                                                *
      *                                                                *
      *    MAPSET       : BVVE024                                      *
      *    MAPA         : BVVE024                                      *
      *    TRANSACAO    : VVL1                                         *
      *                                                                *
      *    NAVEGACAO:                                                  *
      *    CHAMADO POR  : BVVE6523 - CONSULTA POR EVENTO E ISPB EMISSOR*
      *                 : BVVE6524 - CONSULTA MENSAGENS POR INTERVALO  *
      *    CHAMA        : SENH1005 - SENHAS                            *
      *                   BVVE6525 - CONSULTA MENSAGENS DO INTERVALO   *
      *----------------------------------------------------------------*
      *    BCO DE DADOS:                                               *
      *    DB2 TABLE                                   INCLUDE/BOOK    *
      *----------------------------------------------------------------*
      *    BOOKS .:                                                    *
      *    BVVEW355    - AREA DE COMUNICACAO                           *
      *    BVVEWERR    - LAYOUT DE ERROS DO BVVE                       *
      *    I#POOLB6    - AREA CONTENDO A ERRO-AREA                     *
      *----------------------------------------------------------------*
      *
      *    ALTERACOES:                                                 *
      *                                                                *
      *================================================================*
       ENVIRONMENT DIVISION.
      *================================================================*
      *
      *================================================================*
       CONFIGURATION                   SECTION.
      *================================================================*
      *
       SPECIAL-NAMES.
           DECIMAL-POINT               IS COMMA.
      *
      *================================================================*
       DATA DIVISION.
      *================================================================*
      *
      *================================================================*
       WORKING-STORAGE                 SECTION.
      *================================================================*
      *
      *----------------------------------------------------------------*
       77  FILLER                      PIC  X(50)      VALUE
            'INICIO DA WORKING STORAGE SECTION'.
      *----------------------------------------------------------------*
       01 WRK-AUXILIARES.
           05 WRK-PROGRAMA             PIC  X(008)     VALUE 'BVVE6524'.
           05 WRK-BVVE6523             PIC  X(008)     VALUE 'BVVE6523'.
           05 WRK-BVVE6525             PIC  X(008)     VALUE 'BVVE6525'.
           05 WRK-TRANSACAO            PIC  X(004)     VALUE  'VVL1'.
           05 WRK-TRANS-6025           PIC  X(004)     VALUE  'VVL2'.
           05 WRK-EXECUCAO-INICIAL     PIC  X(001)     VALUE SPACES.
           05 WRK-QTDE-EDIT            PIC  Z.ZZZ.ZZZ.Z99 VALUE ZEROS.
           05 WRK-QTDE-EDIT-X          REDEFINES   WRK-QTDE-EDIT
                                       PIC  X(013).
           05 WRK-TOTAL                PIC S9(010) COMP-3 VALUE ZEROS.
           05 WRK-OPCAO-SEL            PIC S9(009) COMP-3 VALUE ZEROS.
           05 WRK-SELECIONADO          PIC S9(003) COMP-3 VALUE ZEROS.
           05 WRK-TOTAL-OCOR           PIC S9(005) COMP-3 VALUE ZEROS.
           05 WRK-RESTO                PIC S9(005) COMP-3 VALUE ZEROS.
           05 WRK-PERIODO              PIC S9(015) COMP-3 VALUE ZEROS.
210917     05 WRK-PERIODO-AUX          PIC S9(005) COMP-3 VALUE ZEROS.
           05 WRK-OCORRENCIAS          PIC S9(009) COMP-3 VALUE ZEROS.
           05 WRK-ABSOLUTA-CORRENTE    PIC S9(015) COMP-3 VALUE ZEROS.
           05 WRK-HORA-INICIAL-ABS-AUX PIC S9(015) COMP-3 VALUE ZEROS.
           05 WRK-ITEM-AUX             PIC  9(003)     VALUE ZEROS.
           05 WRK-ULT-POSICAO          PIC S9(009) COMP-3 VALUE ZEROS.
           05 WRK-FIM                  PIC  X(001)     VALUE  SPACES.
           05 WRK-PAGINA               PIC  9(006) COMP-3 VALUE ZEROS.
           05 WRK-PAGINA-AUX           PIC  9(006)     VALUE ZEROS.
           05 WRK-DATA-INICIAL-AUX     PIC  X(010)     VALUE SPACES.
           05 WRK-ERRO                 PIC  X(001)     VALUE SPACES.
           05 WRK-DATA-ANT-G.
              10 WRK-DIA-ANT           PIC  9(002)     VALUE ZEROS.
              10 WRK-BAR1-DATA-ANT     PIC  X(001)     VALUE '/'.
              10 WRK-MES-ANT           PIC  9(002)     VALUE ZEROS.
              10 WRK-BAR2-DATA-ANT     PIC  X(001)     VALUE '/'.
              10 WRK-ANO-ANT           PIC  9(004)     VALUE ZEROS.
           05 WRK-DATA-ANT             REDEFINES WRK-DATA-ANT-G
                                       PIC  X(010).
           05 WRK-INTERVALO-TELA-G.
              10 WRK-HORA-FIM          PIC  X(005)     VALUE SPACES.
              10 FILLER                PIC  X(003)     VALUE ' A '.
              10 WRK-HORA-INI          PIC  X(005)     VALUE SPACES.
           05 WRK-INTERVALO-TELA       REDEFINES WRK-INTERVALO-TELA-G
                                       PIC  X(013).
           05 WRK-COM-FASE.
              10 FILLER                PIC  X(006)     VALUE 'FASE: '.
              10 WRK-COD-COM-FASE      PIC  9(004)     VALUE ZEROS.
              10 FILLER                PIC  X(001)     VALUE '-'.
              10 WRK-DES-COM-FASE      PIC  X(026)     VALUE SPACES.
              10 FILLER                PIC  X(001)     VALUE SPACES.
              10 FILLER                PIC  X(008)     VALUE 'EVENTO: '.
              10 WRK-COM-EVENTO        PIC  X(010)     VALUE SPACES.
              10 FILLER                PIC  X(001)     VALUE SPACES.
              10 FILLER                PIC  X(014)     VALUE
                 'ISPB EMISSOR: '.
              10 WRK-COM-ISPB-EMI      PIC  X(008)     VALUE SPACES.
           05 WRK-LINHA-COM-FASE       REDEFINES WRK-COM-FASE
                                       PIC  X(079).
           05 WRK-SEM-FASE.
              10 FILLER                PIC  X(008)     VALUE 'EVENTO: '.
              10 WRK-SEM-EVENTO        PIC  X(010)     VALUE SPACES.
              10 FILLER                PIC  X(001)     VALUE SPACES.
              10 FILLER                PIC  X(014)     VALUE
                 'ISPB EMISSOR: '.
              10 WRK-SEM-ISPB-EMI      PIC  X(008)     VALUE SPACES.
              10 FILLER                PIC  X(038)     VALUE SPACES.
           05 WRK-LINHA-SEM-FASE       REDEFINES WRK-SEM-FASE
                                       PIC  X(079).
071117     05 WRK-DT-ATU               PIC  X(010)     VALUE SPACES.
.          05 WRK-BVVEB093             PIC  X(022)     VALUE
.                                      'TRESUL_MNTRC_DIA_CATAO'.
.          05 WRK-BVVEB096             PIC  X(018)     VALUE
071117                                 'TRESUL_TRFGO_MES'.
      *
           05 WRK-DATESTRING-G.
              10 FILLER                PIC  X(005)     VALUE SPACES.
              10 WRK-DIA-DATESTRING    PIC  9(002)     VALUE ZEROS.
              10 FILLER                PIC  X(001)     VALUE SPACES.
              10 WRK-MES-DATESTRING    PIC  X(003)     VALUE SPACES.
              10 FILLER                PIC  X(001)     VALUE SPACES.
              10 WRK-ANO-DATESTRING    PIC  9(004)     VALUE ZEROS.
              10 FILLER                PIC  X(001)     VALUE SPACES.
              10 WRK-HORA-DATESTRING   PIC  X(008)     VALUE SPACES.
              10 WRK-FINAL-DATESTRING  PIC  X(039)     VALUE SPACES.
           05 WRK-DATESTRING           REDEFINES WRK-DATESTRING-G
                                       PIC  X(064).
      *
           05 WRK-STRING-23-G.
              10 FILLER                PIC  X(005)     VALUE 'MON, '.
              10 WRK-DIA-STRING-23     PIC  9(002)     VALUE ZEROS.
              10 FILLER                PIC  X(001)     VALUE SPACES.
              10 WRK-MES-STRING-23     PIC  X(003)     VALUE SPACES.
              10 FILLER                PIC  X(001)     VALUE SPACES.
              10 WRK-ANO-STRING-23     PIC  9(004)     VALUE ZEROS.
              10 FILLER                PIC  X(001)     VALUE SPACES.
              10 WRK-HORA-STRING-23    PIC  X(008)     VALUE SPACES.
              10 WRK-FINAL-STRING-23   PIC  X(039)     VALUE SPACES.
           05 WRK-STRING-23            REDEFINES WRK-STRING-23-G
                                       PIC  X(064).
      *
           05 WRK-TABELA-MES.
              10 FILLER                PIC  X(003)     VALUE 'JAN'.
              10 FILLER                PIC  X(003)     VALUE 'FEB'.
              10 FILLER                PIC  X(003)     VALUE 'MAR'.
              10 FILLER                PIC  X(003)     VALUE 'APR'.
              10 FILLER                PIC  X(003)     VALUE 'MAY'.
              10 FILLER                PIC  X(003)     VALUE 'JUN'.
              10 FILLER                PIC  X(003)     VALUE 'JUL'.
              10 FILLER                PIC  X(003)     VALUE 'AUG'.
              10 FILLER                PIC  X(003)     VALUE 'SEP'.
              10 FILLER                PIC  X(003)     VALUE 'OCT'.
              10 FILLER                PIC  X(003)     VALUE 'NOV'.
              10 FILLER                PIC  X(003)     VALUE 'DEC'.
           05 FILLER                   REDEFINES WRK-TABELA-MES.
              10 WRK-MES-EXT           OCCURS 12 TIMES
                                       PIC  X(003).
      *
.          05 WRK-DTHORA.
.             10 WRK-DD-DTHORA         PIC  9(002)     VALUE ZEROS.
.             10 FILLER                PIC  X(001)     VALUE '/'.
.             10 WRK-MM-DTHORA         PIC  9(002)     VALUE ZEROS.
.             10 FILLER                PIC  X(001)     VALUE '/'.
.             10 WRK-AA-DTHORA         PIC  9(004)     VALUE ZEROS.
.     *
210917     05 WRK-HORASIS              PIC  X(008)     VALUE SPACES.
      *
      *----------------------------------------------------------------*
       77  FILLER                      PIC  X(050)     VALUE
           'AREA DE TS'.
      *----------------------------------------------------------------*
       01 WRK-AREA-TS.
           05 WRK-NOME-TS.
              10 WRK-PROGRAMA-TS      PIC  X(008)     VALUE SPACES.
              10 WRK-TRAN-TS          PIC  X(004)     VALUE 'VVL1'.
              10 WRK-TERM-TS          PIC  X(004)     VALUE SPACES.
      *
           05 WRK-ITEM-TS             PIC S9(004)     COMP VALUE ZEROS.
           05 WRK-ITEM-RETORNO        PIC S9(004)     COMP VALUE ZEROS.
      *
       01 WRK-DADOS-TS.
           05 WRK-QTDOCOR-TS          PIC S9(004) COMP VALUE ZEROS.
           05 WRK-TABELA-TS           OCCURS 10 TIMES.
              10 WRK-HORA-FINAL-ABS   PIC S9(015) COMP-3.
              10 WRK-HORA-INICIAL-ABS PIC S9(015) COMP-3.
              10 WRK-HORA-FINAL-FORMATADA
                                      PIC  X(008).
              10 WRK-HORA-INICIAL-FORMATADA
                                      PIC  X(008).
              10 WRK-QTDE-ENVIADAS    PIC S9(010) COMP-3.
              10 WRK-QTDE-RECEBIDAS   PIC S9(010) COMP-3.
              10 WRK-QTDE-TOTAL       PIC S9(010) COMP-3.
      *
      *----------------------------------------------------------------*
       77  FILLER                      PIC  X(050)     VALUE
                    'AREA DE FORMATACAO DE DATA/HORA'.
      *----------------------------------------------------------------*
       01 WRK-FORMATA-DT-HORA.
           05 WRK-HORA                 PIC  X(008)     VALUE SPACES.
           05 WRK-DATA                 PIC  X(010)     VALUE SPACES.
210917     05 FILLER                   REDEFINES       WRK-DATA.
.             10 WRK-DD-DATA           PIC  9(002).
.             10 FILLER                PIC  X(001).
.             10 WRK-MM-DATA           PIC  9(002).
.             10 FILLER                PIC  X(001).
210917        10 WRK-AA-DATA           PIC  9(004).
           05 WRK-ABSTIME              PIC S9(015) COMP-3 VALUE ZEROS.
           05 WRK-ABSTIME-ANT          PIC S9(015) COMP-3 VALUE ZEROS.
      *
      *----------------------------------------------------------------*
       77  FILLER                      PIC  X(050)     VALUE
           'AREA PARA MENSAGENS'.
      *----------------------------------------------------------------*
       01 WRK-AREA-MENSAGEM.
           05 WRK-MSG-ERRO             PIC  X(079)     VALUE SPACES.
           05 WRK-MSG-ERRO-1           PIC  X(079)     VALUE
               'VVL1 - ACESSO NEGADO, FAVOR ACESSAR PELO SENHAS'.
           05 WRK-MSG-1                PIC  X(079)     VALUE
               'DIGITE "X" NA OPCAO DESEJADA E TECLE <ENTER>'.
           05 WRK-MSG-5                PIC  X(079)     VALUE
           'OPCAO INVALIDA, DIGITE "X" PARA SELECIONAR E TECLE <ENTER>'.
           05 WRK-MSG-6                PIC  X(079)     VALUE
               'SELECIONE APENAS UMA OPCAO'.
           05 WRK-MSG-7                PIC  X(079) VALUE
              'FIM DA AMOSTRAGEM'.
      *
      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(032)     VALUE
           '*        AREA DE SENHAS        *'.
      *----------------------------------------------------------------*
       01  WRK-SENH1005                PIC  X(008)     VALUE 'SENH1005'.
       01  WRK-AREA-SENH1005.
           05  WRK-VERSAO              PIC  9(002)     VALUE 1.
           05  WRK-MENSAGEM.
               10  WRK-COD-MENSAGEM    PIC  9(003)     VALUE ZEROS.
               10  FILLER              PIC  X(001)     VALUE SPACES.
               10  WRK-TEXTO-MENSAGEM  PIC  X(079)     VALUE SPACES.
           05  WRK-SISTEMA             PIC  X(004)     VALUE SPACES.
           05  WRK-NIVEL               PIC  X(001)     VALUE SPACES.
               88  WRK-MENU-SENHAS     VALUE 'P'.
               88  WRK-MENU-BVVE       VALUE 'S'.
           05  WRK-ACAO                PIC  X(001)     VALUE SPACES.
               88  WRK-ENCERRA         VALUE 'E'.
               88  WRK-CONTINUA        VALUE 'C'.
               88  WRK-TRATA-ERRO      VALUE 'T'.
           05  WRK-LINHA-COMANDO       PIC  X(068)     VALUE SPACES.
           05  WRK-IMPRESSORA-1005     PIC  X(008)     VALUE SPACES.
           05  FILLER                  PIC  X(096)     VALUE SPACES.
      *                                                                *
      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050)     VALUE
               'AREA DE COMUNICACAO'.
      *----------------------------------------------------------------*
       01 WRK-COMMAREA.
           COPY 'BVVEW355'.
           10 WRK-COMM-CHAVE-PAG       OCCURS 07 TIMES.
              15 WRK-COMM-PF           PIC  X(003).
              15 WRK-COMM-NUMPAG       PIC S9(005) COMP-3.
              15 WRK-COMM-ULT-PAG      PIC S9(005) COMP-3.
              15 WRK-COMM-ABSTIME      PIC S9(015) COMP-3.
              15 FILLER                PIC  X(083).
           10 WRK-COMM-NOME-TS         PIC  X(016) VALUE SPACES.
      *
      *----------------------------------------------------------------*
       77  FILLER                      PIC  X(050)     VALUE
           'AREA PARA INDEXADORES'.
      *----------------------------------------------------------------*
       77  IND-1                       PIC S9(009) COMP-3 VALUE ZEROS.
       77  IND-2                       PIC S9(009) COMP-3 VALUE ZEROS.
       77  IND-3                       PIC S9(009) COMP-3 VALUE ZEROS.
      *
      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050)     VALUE
           '*        AREA DO TECLADO       *'.
      *----------------------------------------------------------------*
       COPY DFHAID.
      *
      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050)     VALUE
           '*   AREA PARA SETAR ATRIBUTOS  *'.
      *----------------------------------------------------------------*
       COPY DFHBMSCA.
      *                                                                *
      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050)     VALUE
                '*   AREA DE TRATAMENTO DE ERROS *'.
      *----------------------------------------------------------------*
       01 WRK-AREA-ERRO.
           COPY BVVEWERR.
      *                                                                *
      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(32)      VALUE
           '* AREA DE COMUNICACAO CDES0111 *'.
      *----------------------------------------------------------------*
       COPY 'I#POOLB6'.
      *
      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050)     VALUE
           '*          AREA DO MAPA        *'.
      *----------------------------------------------------------------*
       COPY 'BVVE024'.
      *
       01 WRK-MAPA-SAIDA               REDEFINES BVVE024O.
210917      05 FILLER                  PIC  X(042).
            05 WRK-IND-PAGO            PIC  X(001).
            05 FILLER                  PIC  X(004).
            05 WRK-PAGINAO             PIC  9(006).
            05 WRK-DIAL                PIC S9(004) COMP.
            05 WRK-ATRB-DIA            PIC  X(001).
            05 FILLER                  PIC  X(001).
            05 WRK-DIAO                PIC  9(002).
            05 WRK-MESL                PIC S9(004) COMP.
            05 WRK-ATRB-MES            PIC  X(001).
            05 FILLER                  PIC  X(001).
            05 WRK-MESO                PIC  9(002).
            05 WRK-ANOL                PIC S9(004) COMP.
            05 WRK-ATRB-ANO            PIC  X(001).
            05 FILLER                  PIC  X(001).
            05 WRK-ANOO                PIC  9(004).
            05 FILLER                  PIC  X(004).
            05 WRK-STATUSO             PIC  X(032).
            05 FILLER                  PIC  X(004).
            05 WRK-FASEO               PIC  X(079).
            05 FILLER                  OCCURS 10 TIMES.
               10 WRK-SELECAOL         PIC S9(004) COMP.
               10 WRK-ATRB-SELECAO     PIC  X(001).
               10 FILLER               PIC  X(001).
               10 WRK-SELECAOO         PIC  X(001).
               10 FILLER               PIC  X(003).
               10 FILLER               PIC  X(001).
               10 WRK-INTERVO          PIC  X(013).
               10 FILLER               PIC  X(003).
               10 FILLER               PIC  X(001).
               10 WRK-QTDENVO          PIC  X(013).
               10 FILLER               PIC  X(003).
               10 FILLER               PIC  X(001).
               10 WRK-QTDRECO          PIC  X(013).
               10 FILLER               PIC  X(003).
               10 FILLER               PIC  X(001).
               10 WRK-TOTALO           PIC  X(013).
           05 FILLER                   PIC  X(004).
           05 FILLER                   PIC  X(079).
      *                                                                *
      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050)     VALUE
           '* AUXILIARES DAS QUERYS *'.
      *----------------------------------------------------------------*
       01  WRK-AUX-ROWSET.
           05 WRK-GRUPO                PIC  X(003)     VALUE SPACES.
           05 WRK-DATA-TELA            PIC  X(010)     VALUE SPACES.
           05 FILLER                   REDEFINES   WRK-DATA-TELA.
              10 WRK-DIA-TELA          PIC  9(002).
              10 FILLER                PIC  X(001).
              10 WRK-MES-TELA          PIC  9(002).
              10 FILLER                PIC  X(001).
              10 WRK-ANO-TELA          PIC  9(004).
           05 WRK-CORTE-INICIAL        PIC S9(015) COMP-3 VALUE ZEROS.
           05 WRK-CORTE-FINAL          PIC S9(015) COMP-3 VALUE ZEROS.
      *
       01  WRK-LEITURA-ROWSET.
           05 WRK-QMSGEM-ENVID         PIC S9(010) COMP-3 VALUE ZEROS.
           05 WRK-QMSGEM-RECBD         PIC S9(010) COMP-3 VALUE ZEROS.
      *
      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(032)     VALUE
           '*         AREAS DO DB2         *'.
      *----------------------------------------------------------------*
      *
           EXEC SQL
                INCLUDE  SQLCA
           END-EXEC.
      *
071117     EXEC SQL
.              INCLUDE BVVEB093
.          END-EXEC.
.     *
.          EXEC SQL
.              INCLUDE BVVEB096
071117     END-EXEC.
      *
      *----------------------------------------------------------------*
       77  FILLER                      PIC  X(050)     VALUE
               'FIM DA WORKING STORAGE SECTION'.
      *----------------------------------------------------------------*
      *
      *================================================================*
       LINKAGE                         SECTION.
      *================================================================*
       01 DFHCOMMAREA.
           05 FILLER                   PIC  X(1500).
      *
      *================================================================*
       PROCEDURE DIVISION              USING DFHCOMMAREA.
      *================================================================*
      *
      *----------------------------------------------------------------*
      * ROTINA PRINCIPAL DO PROGRAMA                                   *
      *----------------------------------------------------------------*
       0000-PRINCIPAL                  SECTION.
      *----------------------------------------------------------------*
      *
           MOVE '0010'                 TO ERR-LOCAL
      *
           PERFORM 1000-INICIALIZAR
      *
           IF  EIBCALEN                EQUAL ZEROS
               MOVE WRK-MSG-ERRO-1     TO WRK-MSG-ERRO
               PERFORM 5000-ENCERRAR-TRANSACAO
           END-IF
      *
           IF  EIBTRNID                NOT EQUAL WRK-TRANSACAO
               EVALUATE EIBTRNID
                   WHEN WRK-TRANS-6025
                       PERFORM 1200-TRATAR-RETORNO-6025
                   WHEN OTHER
                       PERFORM 1050-RECEBER-AREA-COMUNICACAO
                       MOVE 1          TO WRK-COMM-NUMPAG(5)
               END-EVALUATE
      *
               MOVE WRK-PROGRAMA       TO WRK-PROGRAMA-TS
               MOVE WRK-TRANSACAO      TO WRK-TRAN-TS
               MOVE EIBTRMID           TO WRK-TERM-TS
               MOVE WRK-NOME-TS        TO WRK-COMM-NOME-TS
      *
               PERFORM 1400-EFETUAR-PROCESSO-INICIAL
           END-IF
      *
           PERFORM 1050-RECEBER-AREA-COMUNICACAO
           PERFORM 4000-RECEBER-TELA
      *
           MOVE WRK-COMM-NOME-TS       TO WRK-NOME-TS
      *
           EVALUATE EIBAID
               WHEN DFHPF2
                   PERFORM 4100-TRATAR-PF2
               WHEN DFHPF3
                   PERFORM 4200-TRATAR-PF3
               WHEN DFHPF5
                   PERFORM 4300-TRATAR-PF5
               WHEN DFHPF7
      *            TELA ANTERIOR
                   PERFORM 4400-TRATAR-PF7
               WHEN DFHPF8
      *            PROXIMA TELA
                   PERFORM 4500-TRATAR-PF8
               WHEN DFHPF10
                   PERFORM 4605-TRATAR-PF10
               WHEN DFHENTER
                   PERFORM 4700-TRATAR-ENTER
               WHEN DFHCLEAR
                   MOVE 'VVL1 - FIM DE TRANSACAO'
                                       TO WRK-MSG-ERRO
                   PERFORM 5000-ENCERRAR-TRANSACAO
               WHEN OTHER
                   PERFORM 4800-TRATAR-PF-INVALIDA
           END-EVALUATE
           .
      *----------------------------------------------------------------*
       0000-99-FIM.                    EXIT.
      *----------------------------------------------------------------*
      *
      *----------------------------------------------------------------*
      * ROTINA DE INICIALIZACAO DO PROGRAMA                            *
      *----------------------------------------------------------------*
       1000-INICIALIZAR                SECTION.
      *----------------------------------------------------------------*
      *
           MOVE '0020'                 TO ERR-LOCAL
      *
           EXEC CICS HANDLE
               ABEND
               LABEL (9400-ERRO-CICS)
           END-EXEC.
      *
           INITIALIZE                  ERRO-AREA
                                       WRK-DADOS-TS
      *
           MOVE LOW-VALUES             TO BVVE024O
           MOVE ZEROS                  TO WRK-ITEM-RETORNO
           .
      *----------------------------------------------------------------*
       1000-99-FIM.                    EXIT.
      *----------------------------------------------------------------*
      *
      *----------------------------------------------------------------*
      * ROTINA PARA RECEBER A AREA DE COMUNICACAO NO LAYOUT PRINCIPAL  *
      *----------------------------------------------------------------*
       1050-RECEBER-AREA-COMUNICACAO   SECTION.
      *----------------------------------------------------------------*
      *
           MOVE '0030'                 TO ERR-LOCAL
      *
           MOVE DFHCOMMAREA(1:EIBCALEN)
                                       TO WRK-COMMAREA
           .
      *----------------------------------------------------------------*
       1050-99-FIM.                    EXIT.
      *----------------------------------------------------------------*
      *
      *----------------------------------------------------------------*
      *    INICIALIZA OS CAMPOS DA TELA                                *
      *----------------------------------------------------------------*
       1100-INICIALIZAR-CONTEUDO       SECTION.
      *----------------------------------------------------------------*
      *
           MOVE '0040'                 TO ERR-LOCAL
      *
           PERFORM VARYING IND-1       FROM 1 BY 1
                           UNTIL       IND-1  GREATER 10
               MOVE SPACES             TO WRK-SELECAOO(IND-1)
                                          WRK-INTERVO(IND-1)
                                          WRK-QTDENVO(IND-1)
                                          WRK-QTDRECO(IND-1)
                                          WRK-TOTALO(IND-1)
               MOVE DFHBMPRO           TO WRK-ATRB-SELECAO(IND-1)
           END-PERFORM
      *
           MOVE SPACES                 TO MENSAGO
           .
      *----------------------------------------------------------------*
       1100-99-FIM.                    EXIT.
      *----------------------------------------------------------------*
      *
      *----------------------------------------------------------------*
      *    TRATA O RETORNO DO MODULO BVVE6525(MENSAGENS)               *
      *----------------------------------------------------------------*
       1200-TRATAR-RETORNO-6025        SECTION.
      *----------------------------------------------------------------*
      *
           MOVE '0050'                 TO ERR-LOCAL
      *
           MOVE DFHCOMMAREA(1:EIBCALEN)
                                       TO WRK-COMMAREA
           MOVE ZEROS                  TO WRK-COMM-ULT-PAG(5)
           MOVE 'R'                    TO WRK-EXECUCAO-INICIAL
      *
           .
      *----------------------------------------------------------------*
       1200-99-FIM.                    EXIT.
      *----------------------------------------------------------------*
      *
      *----------------------------------------------------------------*
      *    EFETUA A CONSULTA INICIAL
      *----------------------------------------------------------------*
       1400-EFETUAR-PROCESSO-INICIAL   SECTION.
      *----------------------------------------------------------------*
      *
           MOVE '0060'                 TO ERR-LOCAL
      *
           PERFORM 6000-DELETAR-TS.
      *
           MOVE ZEROS                  TO WRK-COMM-ULT-PAG(5)
           MOVE BVVE355-DINCL-RESUL-MNTRC
                                       TO WRK-DATA-TELA
                                          WRK-DATA-INICIAL-AUX
           MOVE WRK-DIA-TELA           TO WRK-DIAO
           MOVE WRK-MES-TELA           TO WRK-MESO
           MOVE WRK-ANO-TELA           TO WRK-ANOO
      *
           STRING                      BVVE355-CSNLZD-MSGRA
                                       '-'
                                       BVVE355-RSNLZD-MSGRA
                                       DELIMITED BY SIZE
                                       INTO WRK-STATUSO
      *
           IF  BVVE355-CSNLZD-MSGRA    EQUAL 004
               MOVE BVVE355-CFASE-PROCM
                                       TO WRK-COD-COM-FASE
               MOVE BVVE355-RFASE-PROCM
                                       TO WRK-DES-COM-FASE
               STRING BVVE355-CSIST-EXTER
                      BVVE355-CEVNTO-MSGEM(1:3)
                      BVVE355-CFLUXO-MSGEM
                   DELIMITED BY SIZE INTO WRK-COM-EVENTO
               MOVE BVVE355-CISPB-EMISR-MSGEM
                                       TO WRK-COM-ISPB-EMI
               MOVE WRK-LINHA-COM-FASE TO WRK-FASEO
           ELSE
               STRING BVVE355-CSIST-EXTER
                      BVVE355-CEVNTO-MSGEM(1:3)
                      BVVE355-CFLUXO-MSGEM
                   DELIMITED BY SIZE INTO WRK-SEM-EVENTO
               MOVE BVVE355-CISPB-EMISR-MSGEM
                                       TO WRK-SEM-ISPB-EMI
               MOVE WRK-LINHA-SEM-FASE TO WRK-FASEO
           END-IF
      *
      ***  CALCULAR QTDE DE OCORRENCIAS DA TABELA INTERNA
           COMPUTE WRK-TOTAL-OCOR = (24 * 60)
           MOVE BVVE355-PERIODO        TO WRK-PERIODO-AUX
           DIVIDE WRK-TOTAL-OCOR       BY WRK-PERIODO-AUX
           GIVING WRK-OCORRENCIAS      REMAINDER WRK-RESTO
      *
           IF  WRK-RESTO               GREATER ZEROS
               ADD 1                   TO WRK-OCORRENCIAS
           END-IF
      *
           PERFORM 1500-FORMATAR-HOST.
      *
      ***  OBTER HORA ABSOLUTA CORRENTE
           PERFORM 3100-OBTER-DATA-HORA

      ***  CASO DATA CORRENTE UTILZA HORA CORRENTE
      ***  CASO DATA ANTERIOR UTILIZA 23:59:59 DO DIA
           MOVE WRK-DATA-TELA          TO WRK-DATA-ANT
           MOVE '/'                    TO WRK-BAR1-DATA-ANT
                                          WRK-BAR2-DATA-ANT
      *
           IF   WRK-DATA               NOT EQUAL WRK-DATA-ANT
                PERFORM 1410-OBTER-ABSTIME-ANT
           END-IF
      *
           MOVE WRK-ABSTIME            TO WRK-ABSOLUTA-CORRENTE
      *
           IF  WRK-EXECUCAO-INICIAL    EQUAL 'R'
               MOVE WRK-COMM-ABSTIME(5)
                                       TO WRK-ABSTIME
           ELSE
               MOVE WRK-ABSTIME        TO WRK-COMM-ABSTIME(5)
           END-IF
      *
      ***  CONVERTER PERIODO DOS INTERVALOS EM MILISEGUNDOS
           MOVE BVVE355-PERIODO        TO WRK-PERIODO-AUX
           COMPUTE WRK-PERIODO = WRK-PERIODO-AUX * 60 * 1000
      *
      ***  DETERMINAR INTERVALO MAIS RECENTE
           MOVE WRK-ABSOLUTA-CORRENTE  TO WRK-CORTE-FINAL
           COMPUTE WRK-CORTE-INICIAL = WRK-ABSOLUTA-CORRENTE -
                                       WRK-PERIODO
      *
           EXEC CICS FORMATTIME
                     ABSTIME     (WRK-CORTE-INICIAL)
                     DATESEP     ('.')
                     DDMMYYYY    (WRK-DATA-INICIAL-AUX)
           END-EXEC
      *
071117     MOVE WRK-DATA               TO WRK-DT-ATU.
.          MOVE '.'                    TO WRK-DT-ATU(3:1)
.                                         WRK-DT-ATU(6:1).
.     *
.          IF   BVVE355-DINCL-RESUL-MNTRC NOT EQUAL WRK-DT-ATU
.               PERFORM 1810-ACESSAR-BVVEB-MES
071117     ELSE
                PERFORM 1800-ACESSAR-BVVEB
071117     END-IF
      *
           MOVE WRK-COMM-NUMPAG(5)     TO WRK-PAGINAO
           MOVE ZEROS                  TO IND-1
      *
           PERFORM 1900-CALCULAR-TOTAIS-INTERV
             UNTIL WRK-FIM             EQUAL 'S'
                OR WRK-OCORRENCIAS     EQUAL IND-1
      *
           IF   IND-1                  GREATER ZEROS
                PERFORM 6100-GRAVAR-TS
           END-IF
      *
           MOVE WRK-ITEM-TS            TO WRK-COMM-ULT-PAG(5)
      *
           PERFORM 2000-OBTER-LISTA-MSGENS
      *
           IF   WRK-COMM-ULT-PAG(5)    GREATER ZEROS
                MOVE WRK-MSG-1         TO MENSAGO
                MOVE -1                TO WRK-SELECAOL(1)
           END-IF
      *
           PERFORM 3000-ENVIAR-TELA
           .
      *----------------------------------------------------------------*
       1400-99-FIM.                    EXIT.
      *----------------------------------------------------------------*
      *
      *----------------------------------------------------------------*
      *    OBTER ABSTIME DATA ANTERIOR                                 *
      *----------------------------------------------------------------*
       1410-OBTER-ABSTIME-ANT          SECTION.
      *----------------------------------------------------------------*
      *
           MOVE '0070'                 TO ERR-LOCAL
      *
           EXEC CICS FORMATTIME
               ABSTIME     (WRK-ABSTIME)
               DATESTRING  (WRK-DATESTRING) LOCAL
           END-EXEC.
      *
           MOVE WRK-DIA-DATESTRING     TO WRK-DIA-STRING-23
           MOVE WRK-MES-DATESTRING     TO WRK-MES-STRING-23
           MOVE WRK-ANO-DATESTRING     TO WRK-ANO-STRING-23
           MOVE WRK-HORA-DATESTRING    TO WRK-HORA-STRING-23
      *
           MOVE WRK-DIA-ANT            TO WRK-DIA-STRING-23
           MOVE WRK-MES-ANT            TO IND-3
           MOVE WRK-MES-EXT(IND-3)     TO WRK-MES-STRING-23
           MOVE WRK-ANO-ANT            TO WRK-ANO-STRING-23
           MOVE '23:59:59'             TO WRK-HORA-STRING-23
      *
           MOVE WRK-FINAL-DATESTRING   TO WRK-FINAL-STRING-23
      *
           EXEC CICS
               CONVERTTIME
               DATESTRING  (WRK-STRING-23)
               ABSTIME     (WRK-ABSTIME-ANT)
           END-EXEC
      *
           MOVE WRK-ABSTIME-ANT       TO WRK-ABSTIME
      *
           .
      *----------------------------------------------------------------*
       1410-99-FIM.                    EXIT.
      *----------------------------------------------------------------*
      *
      *----------------------------------------------------------------*
      *    FORMATAR HOST                                               *
      *----------------------------------------------------------------*
       1500-FORMATAR-HOST              SECTION.
      *----------------------------------------------------------------*
      *
           MOVE '0080'                 TO ERR-LOCAL
      *
071117     MOVE BVVE355-CSNLZD-MSGRA   TO CSNLZD-MSGRA OF BVVEB093
.                                         CSNLZD-MSGRA OF BVVEB096
.          MOVE BVVE355-DINCL-RESUL-MNTRC
.                                      TO DINCL-RESUL-MNTRC OF BVVEB093
.                                         DINCL-RESUL-MNTRC OF BVVEB096
.          MOVE BVVE355-CFASE-PROCM    TO CFASE-PROCM OF BVVEB093
.                                         CFASE-PROCM OF BVVEB096
.          MOVE BVVE355-CSIST-EXTER    TO CSIST-EXTER-CMARA OF BVVEB093
.                                         CSIST-EXTER-CMARA OF BVVEB096
.          MOVE BVVE355-CEVNTO-MSGEM   TO CEVNTO-EXTER-CMARA OF BVVEB093
.                                         CEVNTO-EXTER-CMARA OF BVVEB096
.          MOVE BVVE355-CFLUXO-MSGEM   TO CFLUXO-EXTER-CMARA OF BVVEB093
.                                         CFLUXO-EXTER-CMARA OF BVVEB096
.          MOVE BVVE355-CISPB-EMISR-MSGEM
.                                      TO CISPB-EMISR-MSGEM OF BVVEB093
071117                                    CISPB-EMISR-MSGEM OF BVVEB096
      *
           .
      *----------------------------------------------------------------*
       1500-99-FIM.                    EXIT.
      *----------------------------------------------------------------*
      *
      *----------------------------------------------------------------*
      *    OBTER QUANTIDADE ENVIADAS E RECEBIDAS                       *
      *----------------------------------------------------------------*
       1800-ACESSAR-BVVEB              SECTION.
      *----------------------------------------------------------------*
      *
           MOVE '0090'                 TO ERR-LOCAL
      *
           IF  BVVE355-CSNLZD-MSGRA    EQUAL 004
               EXEC SQL
                 SELECT SUM(QMSGEM_ENVID)
                       ,SUM(QMSGEM_RECBD)
                   INTO :WRK-QMSGEM-ENVID
                       ,:WRK-QMSGEM-RECBD
071117            FROM DB2PRD.TRESUL_MNTRC_DIA_CATAO
                WHERE  DINCL_RESUL_MNTRC  = :BVVEB093.DINCL-RESUL-MNTRC
                  AND  CSNLZD_MSGRA       = :BVVEB093.CSNLZD-MSGRA
                  AND  CFASE_PROCM        = :BVVEB093.CFASE-PROCM
                  AND  CSIST_EXTER_CMARA  = :BVVEB093.CSIST-EXTER-CMARA
                  AND  CEVNTO_EXTER_CMARA = :BVVEB093.CEVNTO-EXTER-CMARA
                  AND  CFLUXO_EXTER_CMARA = :BVVEB093.CFLUXO-EXTER-CMARA
                  AND  CISPB_EMISR_MSGEM  = :BVVEB093.CISPB-EMISR-MSGEM
                  AND  NRELTV_PROCM_RESUL >  :WRK-CORTE-INICIAL
                  AND  NRELTV_PROCM_RESUL <= :WRK-CORTE-FINAL
                  WITH UR
               END-EXEC
HEX   *
HEX            IF (SQLCODE                 NOT EQUAL ZEROS AND +100
HEX                                                    AND -305) OR
HEX               (SQLWARN0                EQUAL 'W')
HEX                MOVE '0100'             TO ERR-LOCAL
HEX                SET ERR-DB2-SELECT      TO TRUE
HEX                MOVE WRK-BVVEB093       TO BVVEWERR-DB2-TABELA
HEX                PERFORM 9300-ERRO-DB2
HEX                MOVE BVVEWERR-ERR-DB2   TO MENSAGO
HEX                PERFORM 3000-ENVIAR-TELA
HEX            END-IF
HEX   *
HEX            IF (SQLCODE                 EQUAL +100 OR -305)
HEX                MOVE ZEROS              TO WRK-QMSGEM-ENVID
HEX                                           WRK-QMSGEM-RECBD
HEX            END-IF
HEX   *
           ELSE
               EXEC SQL
                 SELECT SUM(QMSGEM_ENVID)
                       ,SUM(QMSGEM_RECBD)
                   INTO :WRK-QMSGEM-ENVID
                       ,:WRK-QMSGEM-RECBD
071117            FROM DB2PRD.TRESUL_MNTRC_DIA_CATAO
                WHERE  DINCL_RESUL_MNTRC  = :BVVEB093.DINCL-RESUL-MNTRC
                  AND  CSNLZD_MSGRA       = :BVVEB093.CSNLZD-MSGRA
                  AND  CSIST_EXTER_CMARA  = :BVVEB093.CSIST-EXTER-CMARA
                  AND  CEVNTO_EXTER_CMARA = :BVVEB093.CEVNTO-EXTER-CMARA
                  AND  CFLUXO_EXTER_CMARA = :BVVEB093.CFLUXO-EXTER-CMARA
                  AND  CISPB_EMISR_MSGEM  = :BVVEB093.CISPB-EMISR-MSGEM
                  AND  NRELTV_PROCM_RESUL >  :WRK-CORTE-INICIAL
                  AND  NRELTV_PROCM_RESUL <= :WRK-CORTE-FINAL
                  WITH UR
               END-EXEC
HEX   *
HEX            IF (SQLCODE                 NOT EQUAL ZEROS AND +100
HEX                                                    AND -305) OR
HEX               (SQLWARN0                EQUAL 'W')
HEX                MOVE '0101'             TO ERR-LOCAL
HEX                SET ERR-DB2-SELECT      TO TRUE
HEX                MOVE WRK-BVVEB093       TO BVVEWERR-DB2-TABELA
HEX                PERFORM 9300-ERRO-DB2
HEX                MOVE BVVEWERR-ERR-DB2   TO MENSAGO
HEX                PERFORM 3000-ENVIAR-TELA
HEX            END-IF
HEX   *
HEX            IF (SQLCODE                 EQUAL +100 OR -305)
HEX                MOVE ZEROS              TO WRK-QMSGEM-ENVID
HEX                                           WRK-QMSGEM-RECBD
HEX            END-IF
HEX   *
           END-IF
      *
           COMPUTE WRK-TOTAL = WRK-QMSGEM-ENVID + WRK-QMSGEM-RECBD
      *
           .
      *----------------------------------------------------------------*
       1800-99-FIM.                    EXIT.
      *----------------------------------------------------------------*
      *
071117*----------------------------------------------------------------*
.     *    OBTER QUANTIDADE ENVIADAS E RECEBIDAS                       *
.     *----------------------------------------------------------------*
.      1810-ACESSAR-BVVEB-MES          SECTION.
.     *----------------------------------------------------------------*
.     *
.          MOVE '0091'                 TO ERR-LOCAL
.     *
.          IF  BVVE355-CSNLZD-MSGRA    EQUAL 004
.              EXEC SQL
.                SELECT SUM(QMSGEM_ENVID)
.                      ,SUM(QMSGEM_RECBD)
.                  INTO :WRK-QMSGEM-ENVID
.                      ,:WRK-QMSGEM-RECBD
.                 FROM DB2PRD.TRESUL_TRFGO_MES
.               WHERE  DINCL_RESUL_MNTRC  = :BVVEB096.DINCL-RESUL-MNTRC
.                 AND  CSNLZD_MSGRA       = :BVVEB096.CSNLZD-MSGRA
.                 AND  CFASE_PROCM        = :BVVEB096.CFASE-PROCM
.                 AND  CSIST_EXTER_CMARA  = :BVVEB096.CSIST-EXTER-CMARA
.                 AND  CEVNTO_EXTER_CMARA = :BVVEB096.CEVNTO-EXTER-CMARA
.                 AND  CFLUXO_EXTER_CMARA = :BVVEB096.CFLUXO-EXTER-CMARA
.                 AND  CISPB_EMISR_MSGEM  = :BVVEB096.CISPB-EMISR-MSGEM
.                 AND  NRELTV_PROCM_RESUL >  :WRK-CORTE-INICIAL
.                 AND  NRELTV_PROCM_RESUL <= :WRK-CORTE-FINAL
.                 WITH UR
.              END-EXEC
HEX   *
HEX            IF (SQLCODE                 NOT EQUAL ZEROS AND +100
HEX                                                    AND -305) OR
HEX               (SQLWARN0                EQUAL 'W')
HEX               MOVE '0092'             TO ERR-LOCAL
HEX               SET ERR-DB2-SELECT      TO TRUE
HEX               MOVE WRK-BVVEB096       TO BVVEWERR-DB2-TABELA
HEX               PERFORM 9300-ERRO-DB2
HEX               MOVE BVVEWERR-ERR-DB2   TO MENSAGO
HEX               PERFORM 3000-ENVIAR-TELA
HEX            END-IF
HEX   *
HEX            IF (SQLCODE                 EQUAL +100 OR -305)
HEX                MOVE ZEROS              TO WRK-QMSGEM-ENVID
HEX                                           WRK-QMSGEM-RECBD
HEX            END-IF
HEX   *
.          ELSE
.              EXEC SQL
.                SELECT SUM(QMSGEM_ENVID)
.                      ,SUM(QMSGEM_RECBD)
.                  INTO :WRK-QMSGEM-ENVID
.                      ,:WRK-QMSGEM-RECBD
.                 FROM DB2PRD.TRESUL_TRFGO_MES
.               WHERE  DINCL_RESUL_MNTRC  = :BVVEB096.DINCL-RESUL-MNTRC
.                 AND  CSNLZD_MSGRA       = :BVVEB096.CSNLZD-MSGRA
.                 AND  CSIST_EXTER_CMARA  = :BVVEB096.CSIST-EXTER-CMARA
.                 AND  CEVNTO_EXTER_CMARA = :BVVEB096.CEVNTO-EXTER-CMARA
.                 AND  CFLUXO_EXTER_CMARA = :BVVEB096.CFLUXO-EXTER-CMARA
.                 AND  CISPB_EMISR_MSGEM  = :BVVEB096.CISPB-EMISR-MSGEM
.                 AND  NRELTV_PROCM_RESUL >  :WRK-CORTE-INICIAL
.                 AND  NRELTV_PROCM_RESUL <= :WRK-CORTE-FINAL
.                 WITH UR
.              END-EXEC
HEX   *
HEX            IF (SQLCODE                 NOT EQUAL ZEROS AND +100
HEX                                                    AND -305) OR
HEX               (SQLWARN0                EQUAL 'W')
HEX               MOVE '0093'             TO ERR-LOCAL
HEX               SET ERR-DB2-SELECT      TO TRUE
HEX               MOVE WRK-BVVEB096       TO BVVEWERR-DB2-TABELA
HEX               PERFORM 9300-ERRO-DB2
HEX               MOVE BVVEWERR-ERR-DB2   TO MENSAGO
HEX               PERFORM 3000-ENVIAR-TELA
HEX            END-IF
HEX   *
HEX            IF (SQLCODE                 EQUAL +100 OR -305)
HEX                MOVE ZEROS              TO WRK-QMSGEM-ENVID
HEX                                           WRK-QMSGEM-RECBD
HEX            END-IF
HEX   *
.          END-IF
HEX   *
.          COMPUTE WRK-TOTAL = WRK-QMSGEM-ENVID + WRK-QMSGEM-RECBD
.     *
.          .
.     *----------------------------------------------------------------*
071117 1810-99-FIM.                    EXIT.
      *----------------------------------------------------------------*
      *
      *----------------------------------------------------------------*
      *    CALCULAR TOTAIS POR INTERVALO                               *
      *----------------------------------------------------------------*
       1900-CALCULAR-TOTAIS-INTERV     SECTION.
      *----------------------------------------------------------------*
      *
           MOVE '0110'                 TO ERR-LOCAL
      *
           ADD 1                       TO IND-1
      *
      **   VALORIZAR TABELA INTERNA
           MOVE WRK-CORTE-FINAL        TO WRK-HORA-FINAL-ABS(IND-1)
           MOVE WRK-CORTE-INICIAL      TO WRK-HORA-INICIAL-ABS(IND-1)
      *
           EXEC CICS FORMATTIME
               ABSTIME     (WRK-HORA-FINAL-ABS(IND-1))
               TIMESEP
               TIME        (WRK-HORA-FINAL-FORMATADA(IND-1))
           END-EXEC.
      *
           COMPUTE WRK-HORA-INICIAL-ABS-AUX =
                   WRK-HORA-INICIAL-ABS(IND-1) + (60 * 1000)

           EXEC CICS FORMATTIME
               ABSTIME     (WRK-HORA-INICIAL-ABS-AUX)
               TIMESEP
               TIME        (WRK-HORA-INICIAL-FORMATADA(IND-1))
           END-EXEC.
      *
           IF  WRK-DATA-INICIAL-AUX    NOT EQUAL WRK-DATA-TELA
               MOVE '00:00'            TO
                                       WRK-HORA-INICIAL-FORMATADA(IND-1)
               MOVE 'S'                TO WRK-FIM
           END-IF
      *
           MOVE WRK-QMSGEM-ENVID       TO WRK-QTDE-ENVIADAS(IND-1)
           MOVE WRK-QMSGEM-RECBD       TO WRK-QTDE-RECEBIDAS(IND-1)
           MOVE WRK-TOTAL              TO WRK-QTDE-TOTAL(IND-1)
      *
           MOVE WRK-CORTE-INICIAL      TO WRK-CORTE-FINAL
      *
           COMPUTE WRK-CORTE-INICIAL = WRK-CORTE-INICIAL - WRK-PERIODO
      *
           IF  IND-1                   EQUAL 10
               PERFORM 6100-GRAVAR-TS
               MOVE ZEROS              TO IND-1
           END-IF
      *
           IF  WRK-FIM                 EQUAL 'S' AND
               IND-1                   GREATER ZEROS
               PERFORM 6100-GRAVAR-TS
               MOVE ZEROS              TO IND-1
           ELSE
               EXEC CICS FORMATTIME
                    ABSTIME     (WRK-CORTE-INICIAL)
                    DATESEP     ('.')
                    DDMMYYYY    (WRK-DATA-INICIAL-AUX)
               END-EXEC
      *
071117         MOVE WRK-DATA           TO WRK-DT-ATU
.              MOVE '.'                TO WRK-DT-ATU(3:1)
.                                         WRK-DT-ATU(6:1)
.     *
.              IF   BVVE355-DINCL-RESUL-MNTRC
.                                      NOT EQUAL WRK-DT-ATU
.                  PERFORM 1810-ACESSAR-BVVEB-MES
071117         ELSE
                   PERFORM 1800-ACESSAR-BVVEB
071117         END-IF
           END-IF
      *
           .
      *----------------------------------------------------------------*
       1900-99-FIM.                    EXIT.
      *----------------------------------------------------------------*
      *
      *----------------------------------------------------------------*
      *    OBTER A LISTA DE STATUS MENSAGENS                           *
      *----------------------------------------------------------------*
       2000-OBTER-LISTA-MSGENS         SECTION.
      *----------------------------------------------------------------*
      *
           MOVE '0120'                 TO ERR-LOCAL
      *
           PERFORM 2600-TRATAR-LISTA
           .
      *----------------------------------------------------------------*
       2000-99-FIM.                    EXIT.
      *----------------------------------------------------------------*
      *
      *----------------------------------------------------------------*
      *    TRATAR A LISTA OBTIDA                                       *
      *----------------------------------------------------------------*
       2600-TRATAR-LISTA               SECTION.
      *----------------------------------------------------------------*
      *
           MOVE '0130'                 TO ERR-LOCAL
      *
           MOVE ZEROS                  TO IND-2
           MOVE WRK-COMM-ULT-PAG(5)    TO WRK-PAGINA-AUX
      *
           IF  WRK-PAGINAO             EQUAL WRK-PAGINA-AUX
               MOVE ' '                TO WRK-IND-PAGO
           ELSE
               MOVE '+'                TO WRK-IND-PAGO
           END-IF
      *
           MOVE WRK-PAGINAO            TO WRK-ITEM-TS
           PERFORM 6200-LER-TS
           MOVE WRK-QTDOCOR-TS         TO WRK-ULT-POSICAO
      *
           PERFORM VARYING IND-1       FROM  1 BY 1
                     UNTIL IND-2       EQUAL WRK-ULT-POSICAO
                        OR IND-2       EQUAL 10
      *
               ADD 1                   TO IND-2
               MOVE WRK-HORA-INICIAL-FORMATADA (IND-1)
                                       TO WRK-HORA-INI
               MOVE WRK-HORA-FINAL-FORMATADA (IND-1)
                                       TO WRK-HORA-FIM
               MOVE WRK-INTERVALO-TELA TO WRK-INTERVO(IND-2)
               MOVE WRK-QTDE-ENVIADAS(IND-1)
                                       TO WRK-QTDE-EDIT
               MOVE WRK-QTDE-EDIT-X    TO WRK-QTDENVO(IND-2)
      *
               MOVE WRK-QTDE-RECEBIDAS(IND-1)
                                       TO WRK-QTDE-EDIT
               MOVE WRK-QTDE-EDIT-X    TO WRK-QTDRECO(IND-2)
      *
               MOVE WRK-QTDE-TOTAL(IND-1)
                                       TO WRK-QTDE-EDIT
               MOVE WRK-QTDE-EDIT-X    TO WRK-TOTALO(IND-2)
      *
210917         IF  WRK-QTDE-TOTAL(IND-1)
.                                      EQUAL ZEROS
.                  MOVE SPACES         TO WRK-QTDENVO(IND-2)
.                                         WRK-QTDRECO(IND-2)
210917         END-IF
      *
           END-PERFORM
      *
           .
      *----------------------------------------------------------------*
       2600-99-FIM.                    EXIT.
      *----------------------------------------------------------------*
      *
      *----------------------------------------------------------------*
      *----------------------------------------------------------------*
      *    TRATA SELECAO                                               *
      *----------------------------------------------------------------*
       2800-TRATAR-SELECAO             SECTION.
      *----------------------------------------------------------------*
      *
           MOVE '0140'                 TO ERR-LOCAL
      *
           MOVE 'N'                    TO WRK-FIM
           MOVE ZEROS                  TO WRK-OPCAO-SEL
                                          WRK-SELECIONADO
      *
           PERFORM VARYING IND-1       FROM 1 BY 1
                     UNTIL IND-1       GREATER 10
                        OR WRK-FIM     EQUAL 'S'
      *
               EVALUATE WRK-SELECAOO(IND-1)
                   WHEN SPACES
                   WHEN LOW-VALUES
                       CONTINUE
                   WHEN 'X'
                   WHEN 'X'
                       MOVE -1         TO WRK-SELECAOL(IND-1)
                       MOVE DFHUNIMD   TO WRK-ATRB-SELECAO(IND-1)
                       ADD 1           TO WRK-SELECIONADO
                       MOVE IND-1      TO WRK-OPCAO-SEL
                   WHEN OTHER
                       MOVE -1         TO WRK-SELECAOL(IND-1)
                       MOVE DFHUNIMD   TO WRK-ATRB-SELECAO(IND-1)
                       MOVE 'S'        TO WRK-ERRO
                       MOVE IND-1      TO WRK-OPCAO-SEL
               END-EVALUATE
           END-PERFORM
      *
           IF (WRK-SELECIONADO         GREATER 1) OR
              (WRK-SELECIONADO         EQUAL   1  AND
               WRK-ERRO                EQUAL  'S')
               MOVE 'S'                TO WRK-ERRO
               MOVE WRK-MSG-6          TO MENSAGO
           ELSE
               IF WRK-ERRO             EQUAL 'S'
                  MOVE WRK-MSG-5       TO MENSAGO
               END-IF
           END-IF

           IF (WRK-OPCAO-SEL           GREATER ZEROS AND
               WRK-ERRO                NOT EQUAL 'S')
               MOVE WRK-INTERVO(WRK-OPCAO-SEL)
                                       TO WRK-GRUPO
           ELSE
               IF MENSAGO              EQUAL SPACES OR LOW-VALUES
                  MOVE -1              TO WRK-SELECAOL(1)
                  MOVE WRK-MSG-1       TO MENSAGO
               ELSE
                  MOVE -1              TO WRK-SELECAOL(WRK-OPCAO-SEL)
               END-IF
               PERFORM 3000-ENVIAR-TELA
           END-IF
           .
      *----------------------------------------------------------------*
       2800-99-FIM.                    EXIT.
      *----------------------------------------------------------------*
      *
      *----------------------------------------------------------------*
      *    PASSA O CONTROLE PARA O PROGRAMA BVVE6525                   *
      *----------------------------------------------------------------*
       2900-CHAMAR-MENSAGENS           SECTION.
      *----------------------------------------------------------------*
      *
           MOVE '0150'                 TO ERR-LOCAL
      *
           MOVE WRK-PAGINAO            TO WRK-ITEM-TS
                                          WRK-COMM-NUMPAG(5)
           PERFORM 6200-LER-TS.
      *
           MOVE WRK-PROGRAMA           TO BVVE355-PROGRAMA-CHAMADOR
      *
           MOVE WRK-HORA-INICIAL-ABS(WRK-OPCAO-SEL)
                                       TO BVVE355-HINTERVALO-INI-ABS
           MOVE WRK-HORA-FINAL-ABS(WRK-OPCAO-SEL)
                                       TO BVVE355-HINTERVALO-FIM-ABS
           MOVE WRK-HORA-INICIAL-FORMATADA(WRK-OPCAO-SEL)
                                       TO BVVE355-HINTERVALO-INI
           MOVE WRK-HORA-FINAL-FORMATADA(WRK-OPCAO-SEL)
                                       TO BVVE355-HINTERVALO-FIM
      *
           PERFORM 6000-DELETAR-TS.
      *
           EXEC CICS XCTL
               PROGRAM  (WRK-BVVE6525)
               COMMAREA (WRK-COMMAREA)
               LENGTH   (LENGTH OF WRK-COMMAREA)
           END-EXEC.
      *
           IF (EIBRESP                 NOT EQUAL DFHRESP(NORMAL))
               CONTINUE
           END-IF
      *
           EXEC CICS
               RETURN
           END-EXEC
           .
      *----------------------------------------------------------------*
       2900-99-FIM.                    EXIT.
      *----------------------------------------------------------------*
      *
      *----------------------------------------------------------------*
      *    ENVIA INTERFACE PARA INTERACAO COM USUARIO                  *
      *----------------------------------------------------------------*
       3000-ENVIAR-TELA                SECTION.
      *----------------------------------------------------------------*
      *
           MOVE '0160'                 TO ERR-LOCAL
      *
           PERFORM 3100-OBTER-DATA-HORA
      *
           PERFORM 3200-FORMATAR-ATRIBUTO
      *
           EXEC CICS SEND
               MAP    ('BVVE024')
               MAPSET ('BVVE024')
               FROM   (BVVE024O)
               ERASE
               CURSOR
               FREEKB
           END-EXEC.
      *
           EXEC CICS RETURN
               TRANSID  ('VVL1')
               COMMAREA (WRK-COMMAREA)
               LENGTH   (LENGTH OF WRK-COMMAREA)
           END-EXEC
           .
      *----------------------------------------------------------------*
       3000-99-FIM.                    EXIT.
      *----------------------------------------------------------------*
      *
      *----------------------------------------------------------------*
      *    OBTEM E FORMATA DATA E HORA CORRENTE                        *
      *----------------------------------------------------------------*
       3100-OBTER-DATA-HORA            SECTION.
      *----------------------------------------------------------------*
      *
           MOVE '0170'                 TO ERR-LOCAL
      *
           EXEC CICS ASKTIME
               ABSTIME     (WRK-ABSTIME)
           END-EXEC.
      *
           EXEC CICS FORMATTIME
               ABSTIME     (WRK-ABSTIME)
               DATESEP
               DDMMYYYY    (WRK-DATA)
               TIMESEP
               TIME        (WRK-HORA)
           END-EXEC.
      *
210917     MOVE WRK-HORA(1:8)          TO WRK-HORASIS
           MOVE WRK-HORASIS            TO HORASISO
.     *
.          MOVE WRK-DD-DATA            TO WRK-DD-DTHORA
.          MOVE WRK-MM-DATA            TO WRK-MM-DTHORA
.          MOVE WRK-AA-DATA            TO WRK-AA-DTHORA
210917     MOVE WRK-DTHORA             TO DTHORAO
           .
      *----------------------------------------------------------------*
       3100-99-FIM.                    EXIT.
      *----------------------------------------------------------------*
      *
      *----------------------------------------------------------------*
      *    FORMATAR ATRIBUTOS DA TELA                                  *
      *----------------------------------------------------------------*
       3200-FORMATAR-ATRIBUTO          SECTION.
      *----------------------------------------------------------------*
      *
           MOVE '0180'                 TO ERR-LOCAL
      *
           PERFORM VARYING IND-1 FROM 1 BY 1
                     UNTIL IND-1 GREATER 10
      *
               IF  WRK-INTERVO(IND-1)  EQUAL SPACES OR LOW-VALUES
                   MOVE DFHBMPRO       TO WRK-ATRB-SELECAO(IND-1)
               ELSE
                   IF WRK-ERRO         NOT EQUAL 'S'
                      MOVE DFHALL      TO WRK-ATRB-SELECAO(IND-1)
                   END-IF
               END-IF
           END-PERFORM
      *
           .
      *----------------------------------------------------------------*
       3200-99-FIM.                    EXIT.
      *----------------------------------------------------------------*
      *
      *----------------------------------------------------------------*
      *    RECEBER AS INFORMACOES DA TELA DE INTERACAO COM O USUARIO   *
      *----------------------------------------------------------------*
       4000-RECEBER-TELA               SECTION.
      *----------------------------------------------------------------*
      *
           MOVE '0190'                 TO ERR-LOCAL
      *
           EXEC CICS RECEIVE
               MAP      ('BVVE024')
               MAPSET   ('BVVE024')
               INTO     (BVVE024I)
               NOHANDLE
           END-EXEC.
      *
           IF (EIBRESP                 NOT EQUAL ZEROS AND 36)
               MOVE '0200'             TO ERR-LOCAL
               PERFORM 9400-ERRO-CICS
               MOVE 'VVL1 - ERRO AO RECEBER TELA'
                                       TO WRK-MSG-ERRO
               PERFORM 5000-ENCERRAR-TRANSACAO
           END-IF
      *
           MOVE SPACES                 TO MENSAGO
           .
      *----------------------------------------------------------------*
       4000-99-FIM.                    EXIT.
      *----------------------------------------------------------------*
      *
      *----------------------------------------------------------------*
      *    DESCONECTA                                                  *
      *----------------------------------------------------------------*
       4100-TRATAR-PF2                 SECTION.
      *----------------------------------------------------------------*
      *
           MOVE '0210'                 TO ERR-LOCAL
      *
           PERFORM 6000-DELETAR-TS
      *
           EXEC CICS ISSUE
               DISCONNECT
           END-EXEC.
      *
           EXEC CICS
               RETURN
           END-EXEC.
      *----------------------------------------------------------------*
       4100-99-FIM.                    EXIT.
      *----------------------------------------------------------------*
      *
      *----------------------------------------------------------------*
      *    VOLTA A TELA ANTERIOR                                       *
      *----------------------------------------------------------------*
       4200-TRATAR-PF3                 SECTION.
      *----------------------------------------------------------------*
      *
           MOVE '0220'                 TO ERR-LOCAL
      *
           PERFORM 6000-DELETAR-TS
      *
           MOVE WRK-PROGRAMA           TO BVVE355-PROGRAMA-CHAMADOR
      *
           EXEC CICS XCTL
               PROGRAM  (WRK-BVVE6523)
               COMMAREA (WRK-COMMAREA)
               LENGTH   (LENGTH OF WRK-COMMAREA)
           END-EXEC.
      *
           IF (EIBRESP                 NOT EQUAL DFHRESP(NORMAL))
               CONTINUE
           END-IF
      *
           EXEC CICS
               RETURN
           END-EXEC
           .
      *----------------------------------------------------------------*
       4200-99-FIM.                    EXIT.
      *----------------------------------------------------------------*
      *
      *----------------------------------------------------------------*
      *    VOLTA AO MENU DE ROTINAS                                    *
      *----------------------------------------------------------------*
       4300-TRATAR-PF5                 SECTION.
      *----------------------------------------------------------------*
      *
           MOVE '0230'                 TO ERR-LOCAL
      *
           PERFORM 6000-DELETAR-TS
      *
           SET   WRK-MENU-SENHAS       TO  TRUE
           MOVE  'BVVE'                TO  WRK-SISTEMA
           MOVE  1                     TO  WRK-VERSAO
      *
           EXEC CICS XCTL
               PROGRAM  (WRK-SENH1005)
               COMMAREA (WRK-AREA-SENH1005)
               LENGTH   (LENGTH OF WRK-AREA-SENH1005)
               NOHANDLE
           END-EXEC.
      *
           IF (EIBRESP                 NOT EQUAL DFHRESP(NORMAL))
               CONTINUE
           END-IF
      *
           IF  WRK-ENCERRA
               EXEC CICS
                   RETURN
               END-EXEC
           ELSE
               PERFORM 3000-ENVIAR-TELA
           END-IF
           .
      *----------------------------------------------------------------*
       4300-99-FIM.                    EXIT.
      *----------------------------------------------------------------*
      *
      *----------------------------------------------------------------*
      * CARREGA PAGINA ANTERIOR                                        *
      *----------------------------------------------------------------*
       4400-TRATAR-PF7                 SECTION.
      *----------------------------------------------------------------*
      *
           MOVE '0240'                 TO ERR-LOCAL
      *
           IF WRK-PAGINAO              NOT NUMERIC
               PERFORM 4800-TRATAR-PF-INVALIDA
           END-IF
      *
           MOVE WRK-PAGINAO            TO WRK-PAGINA
      *
           IF WRK-PAGINA               EQUAL 1
               CONTINUE
           ELSE
               MOVE WRK-INTERVO(1)      TO WRK-GRUPO
               PERFORM 1100-INICIALIZAR-CONTEUDO
               COMPUTE WRK-PAGINA = (WRK-PAGINA - 1)
               MOVE WRK-PAGINA         TO WRK-PAGINAO
               PERFORM 2000-OBTER-LISTA-MSGENS
           END-IF
      *
           MOVE WRK-MSG-1              TO MENSAGO
           MOVE -1                     TO WRK-SELECAOL(1)
           PERFORM 3000-ENVIAR-TELA
           .
      *----------------------------------------------------------------*
       4400-99-FIM.                    EXIT.
      *----------------------------------------------------------------*
      *
      *----------------------------------------------------------------*
      * CARREGA PAGINA POSTERIOR                                       *
      *----------------------------------------------------------------*
       4500-TRATAR-PF8                 SECTION.
      *----------------------------------------------------------------*
      *
           MOVE '0250'                 TO ERR-LOCAL
      *
           IF WRK-PAGINAO              NOT NUMERIC
               PERFORM 4800-TRATAR-PF-INVALIDA
           END-IF
      *
           IF WRK-IND-PAGO             EQUAL '+'
               MOVE WRK-PAGINAO        TO WRK-PAGINA
               PERFORM 1100-INICIALIZAR-CONTEUDO
               COMPUTE WRK-PAGINA = (WRK-PAGINA + 1)
               MOVE WRK-PAGINA         TO WRK-PAGINAO
               PERFORM 2000-OBTER-LISTA-MSGENS
               MOVE -1                 TO WRK-SELECAOL(1)
               MOVE WRK-MSG-1          TO MENSAGO
           ELSE
               MOVE WRK-MSG-7          TO MENSAGO
               MOVE -1                 TO WRK-SELECAOL(1)
           END-IF
      *
           PERFORM 3000-ENVIAR-TELA
           .
      *----------------------------------------------------------------*
       4500-99-FIM.                    EXIT.
      *----------------------------------------------------------------*
      *
      *----------------------------------------------------------------*
      *    VOLTA AO MENU DO BVVE                                       *
      *----------------------------------------------------------------*
       4605-TRATAR-PF10                SECTION.
      *----------------------------------------------------------------*
      *
           MOVE '0260'                 TO ERR-LOCAL
      *
           PERFORM 6000-DELETAR-TS
      *
           SET   WRK-MENU-BVVE         TO  TRUE
           MOVE  'BVVE'                TO  WRK-SISTEMA
           MOVE  1                     TO  WRK-VERSAO
      *
           EXEC CICS XCTL
               PROGRAM  (WRK-SENH1005)
               COMMAREA (WRK-AREA-SENH1005)
               LENGTH   (LENGTH OF WRK-AREA-SENH1005)
               NOHANDLE
           END-EXEC.
      *
           IF (EIBRESP                 NOT EQUAL DFHRESP(NORMAL))
               CONTINUE
           END-IF
      *
           IF  WRK-ENCERRA
               EXEC CICS
                   RETURN
               END-EXEC
           ELSE
               PERFORM 3000-ENVIAR-TELA
           END-IF
           .
      *----------------------------------------------------------------*
       4605-99-FIM.                    EXIT.
      *----------------------------------------------------------------*
      *
      *----------------------------------------------------------------*
      *    TRATAMENTO DO E N T E R                                     *
      *----------------------------------------------------------------*
       4700-TRATAR-ENTER               SECTION.
      *----------------------------------------------------------------*
      *
           MOVE '0270'                 TO ERR-LOCAL
      *
           PERFORM 2800-TRATAR-SELECAO
           PERFORM 2900-CHAMAR-MENSAGENS
      *
           .
      *----------------------------------------------------------------*
       4700-99-FIM.                    EXIT.
      *----------------------------------------------------------------*
      *
      *----------------------------------------------------------------*
      *    TRATA PF INVALIDA                                           *
      *----------------------------------------------------------------*
       4800-TRATAR-PF-INVALIDA         SECTION.
      *----------------------------------------------------------------*
      *
           MOVE '0280'                 TO ERR-LOCAL
      *
           MOVE  'PF INVALIDA'         TO MENSAGO
      *
           IF  WRK-INTERVO(1)          NOT EQUAL SPACES OR LOW-VALUES
               MOVE -1                 TO WRK-SELECAOL(1)
           END-IF
           PERFORM 3000-ENVIAR-TELA
           .
      *----------------------------------------------------------------*
       4800-99-FIM.                    EXIT.
      *----------------------------------------------------------------*
      *
      *----------------------------------------------------------------*
      *    ROTINA PARA ENCERRAR TRANSACAO                              *
      *----------------------------------------------------------------*
       5000-ENCERRAR-TRANSACAO         SECTION.
      *----------------------------------------------------------------*
      *
           MOVE '0290'                 TO ERR-LOCAL
      *
           EXEC CICS SEND
               FROM (WRK-MSG-ERRO)
               ERASE
           END-EXEC.
      *
           EXEC CICS
               RETURN
           END-EXEC.
      *
      *----------------------------------------------------------------*
       5000-99-FIM.                    EXIT.
      *----------------------------------------------------------------*
      *                                                                *
      *----------------------------------------------------------------*
      *    DELETAR TS                                                  *
      *----------------------------------------------------------------*
       6000-DELETAR-TS                 SECTION.
      *----------------------------------------------------------------*
      *
           MOVE '0300'                 TO ERR-LOCAL
      *
           EXEC CICS
                DELETEQ TS QNAME(WRK-NOME-TS)
                NOHANDLE
           END-EXEC
      *
           IF (EIBRESP                 NOT EQUAL ZEROS AND 44)
               MOVE '0310'             TO ERR-LOCAL
               PERFORM 9400-ERRO-CICS
               STRING 'VVL1 - ERRO AO DELETAR TS: '
                      WRK-NOME-TS
                      DELIMITED BY SIZE INTO WRK-MSG-ERRO
               PERFORM 5000-ENCERRAR-TRANSACAO
           END-IF
      *
           .
      *----------------------------------------------------------------*
       6000-99-FIM.                    EXIT.
      *----------------------------------------------------------------*
      *
      *----------------------------------------------------------------*
      *    GRAVAR TS                                                   *
      *----------------------------------------------------------------*
       6100-GRAVAR-TS                  SECTION.
      *----------------------------------------------------------------*
      *
           MOVE '0320'                 TO ERR-LOCAL
      *
           MOVE IND-1                  TO WRK-QTDOCOR-TS
      *
           EXEC CICS
                WRITEQ TS QNAME (WRK-NOME-TS)
                     FROM (WRK-DADOS-TS)
                     LENGTH (LENGTH OF WRK-DADOS-TS)
                     ITEM (WRK-ITEM-TS)
                NOHANDLE
           END-EXEC
      *
           IF (EIBRESP                 NOT EQUAL ZEROS)
               MOVE '0330'             TO ERR-LOCAL
               PERFORM 9400-ERRO-CICS
               STRING 'VVL1 - ERRO AO GRAVAR TS: '
                      WRK-NOME-TS
                      DELIMITED BY SIZE INTO WRK-MSG-ERRO
               PERFORM 5000-ENCERRAR-TRANSACAO
           END-IF
      *
           INITIALIZE WRK-DADOS-TS
      *
           .
      *----------------------------------------------------------------*
       6100-99-FIM.                    EXIT.
      *----------------------------------------------------------------*
      *
      *----------------------------------------------------------------*
      *    LER TS                                                      *
      *----------------------------------------------------------------*
       6200-LER-TS                     SECTION.
      *----------------------------------------------------------------*
      *
           MOVE '0340'                 TO ERR-LOCAL
      *
           EXEC CICS
                READQ TS QNAME (WRK-NOME-TS)
                     INTO (WRK-DADOS-TS)
                     ITEM (WRK-ITEM-TS)
                     LENGTH (LENGTH OF WRK-DADOS-TS)
                NOHANDLE
           END-EXEC
      *
           IF (EIBRESP                 NOT EQUAL ZEROS)
               MOVE '0350'             TO ERR-LOCAL
               MOVE WRK-ITEM-TS        TO WRK-ITEM-AUX
               PERFORM 9400-ERRO-CICS
               STRING 'VVL1 - ERRO AO LER TS: '
                      WRK-NOME-TS
                      ' - ITEM:'
                      WRK-ITEM-AUX
                      DELIMITED BY SIZE INTO WRK-MSG-ERRO
               PERFORM 5000-ENCERRAR-TRANSACAO
           END-IF
      *
           .
      *----------------------------------------------------------------*
       6200-99-FIM.                    EXIT.
      *----------------------------------------------------------------*
      *
      *----------------------------------------------------------------*
      * ROTINA PARA GRAVAR LOG DE ERROS                                *
      *----------------------------------------------------------------*
       9000-GERAR-LOG-ERRO             SECTION.
      *----------------------------------------------------------------*
      *
           MOVE 'VRS001'               TO ERR-VERSAO
           MOVE WRK-PROGRAMA           TO ERR-PGM
           MOVE DFHEIBLK               TO ERR-DFHEIBLK
           MOVE SPACES                 TO ERR-SEGM
      *
           EXEC CICS ASSIGN
               APPLID      (ERR-CICS)
           END-EXEC.
      *
           EXEC CICS START
               TRANSID     ('CD11')
               FROM        (ERRO-AREA)
               LENGTH      (LENGTH OF ERRO-AREA)
               NOHANDLE
           END-EXEC.
      *
           IF (EIBRESP                 NOT EQUAL DFHRESP(NORMAL))
               CONTINUE
           END-IF
           .
      *----------------------------------------------------------------*
       9000-99-FIM.                    EXIT.
      *----------------------------------------------------------------*
      *                                                                *
      *----------------------------------------------------------------*
      * ROTINA PARA TRATAMENTO DE ERRO DB2                             *
      *----------------------------------------------------------------*
       9300-ERRO-DB2                   SECTION.
      *----------------------------------------------------------------*
      *
           MOVE 'DB2'                  TO ERR-TIPO-ACESSO
           MOVE EIBTRMID               TO ERR-TERMINAL
           MOVE WRK-PROGRAMA           TO ERR-PGM
                                          BVVEWERR-DB2-MODULO
           MOVE SQLCODE                TO BVVEWERR-DB2-SQLCODE
           MOVE ERR-LOCAL              TO BVVEWERR-DB2-LOCAL
      *
           MOVE BVVEWERR-DB2-TABELA    TO ERR-DBD-TAB
           MOVE BVVEWERR-DB2-COMANDO   TO ERR-FUN-COMANDO
           MOVE SQLCODE                TO ERR-SQL-CODE
           MOVE SQLCA                  TO ERR-SQLCA
           MOVE BVVEWERR-ERR-DB2       TO ERR-TEXTO-MSG
           PERFORM 9000-GERAR-LOG-ERRO
           .
      *----------------------------------------------------------------*
       9300-DB2-FIM.                   EXIT.
      *----------------------------------------------------------------*
      *                                                                *
      *----------------------------------------------------------------*
      *    ROTINA PARA TRATAMENTO DE ERRO CICS.                        *
      *----------------------------------------------------------------*
       9400-ERRO-CICS                  SECTION.
      *----------------------------------------------------------------*
      *
           MOVE 'CIC'                  TO ERR-TIPO-ACESSO
           MOVE DFHEIBLK               TO ERR-DFHEIBLK
           MOVE WRK-PROGRAMA           TO ERR-PGM
                                          BVVEWERR-CICS-MODULO
           MOVE EIBRESP                TO BVVEWERR-CICS-EIBRESP
           MOVE EIBRESP2               TO BVVEWERR-CICS-EIBRESP2
           MOVE ERR-LOCAL              TO BVVEWERR-CICS-LOCAL
      *
           MOVE BVVEWERR-ERR-CICS      TO ERR-TEXTO-MSG
      *
           MOVE EIBTRMID               TO ERR-TERMINAL
           PERFORM 9000-GERAR-LOG-ERRO
           .
      *----------------------------------------------------------------*
       9400-99-FIM.                    EXIT.
      *----------------------------------------------------------------*
