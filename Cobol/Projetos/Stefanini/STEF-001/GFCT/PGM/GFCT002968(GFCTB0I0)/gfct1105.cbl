      *================================================================*
       IDENTIFICATION                  DIVISION.
      *================================================================*
       PROGRAM-ID. GFCT1105.
       AUTHOR.     FELIPE ALEX SOMMER.
      *================================================================*
      *                    C P M  -  S I S T E M A S                   *
      *----------------------------------------------------------------*
      *    PROGRAMA....:   GFCT1105                                    *
      *    PROGRAMADOR.:   FELIPE ALEX SOMMER      - CPM/PATO BRANCO   *
      *    ANALISTA CPM:   KHARUZO I. LEITE        - CPM/PATO BRANCO   *
      *    ANALISTA....:   RODRIGO PRADO           - PWI/GRUPO 67      *
      *    DATA........:   10/11/2004                                  *
      *----------------------------------------------------------------*
      *    OBJETIVO....:   PROCESSAR A  CONSISTENCIA  LOGICA  DOS      *
      *                    ARQUIVOS  INTERFACES  PADRAO  ENVIADOS      *
      *                    AO SISTEMA GFCT PARA GESTAO E COBRANCA      *
      *                    DE TARIFAS.                                 *
      *----------------------------------------------------------------*
      *    ARQUIVOS....:                                               *
      *                DDNAME                      INCLUDE/BOOK        *
      *                  GFCTSEQE                    I#GFCT05          *
      *                  GFCTMVTA                    GFCTWBD2          *
      *                  GFCTMVOK                    GFCTWBD2          *
      *                  GFCTMVRJ                    GFCTWBD2          *
      *                  GFCTMVER                    GFCTWBD3          *
      *                  GFCTSEQS                    I#GFCT05          *
      *----------------------------------------------------------------*
      *    BCO DE DADOS:                                               *
      *                DB2                                             *
      *                  TABLE                       INCLUDE/BOOK      *
      *                    DB2PRD.V01CTRO_CUST         ANPRV005        *
      *                    DB2PRD.MOVTO_ROTNA_ORIGE    GFCTB094        *
      *                    DB2PRD.PARM_SERVC_TARIF     GFCTB0A2        *
      *                    DB2PRD.V01CTA_CORRETE       CLIEV007        *
      *                    DB2PRD.V01CTA_CORRETE       CLIEV008        *
      *                    DB2PRD.V01CLIENTE_AGENCIA   CLIEV004        *
      *                    DB2PRD.TPO_BONIF_SERVC      GFCTB0F4        *
      *                    DB2PRD.PARM_DATA_PROCM      GFCTB0A1        *
      ******************** DB2PRD.PDIDO_FLEXZ_AGPTO    GFCTB0B2        *
      ******************** DB2PRD.FLEXZ_NRO_CONTR      GFCTB059        *
      ******************** DB2PRD.FLEXZ_AGPTO_CTA      GFCTB049        *
      *----------------------------------------------------------------*
      *    INC'S.......:                                               *
      *    I#SCCE91 - AREA CHAMADA SCCE9100.                           *
      *    I#MESUM6 - AREA CHAMADA MESU9015.                           *
      *    I#MUSUM8 - AREA CHAMADA MESU9018.                           *
      *    I#NTAX00 - AREA CHAMADA NTAX5000.                           *
      *    I#POOLB4 - AREA CHAMADA POOL0536.                           *
      *    POL7100C - AREA CHAMADA POOL7100 - CANCELAMENTO.            *
      *----------------------------------------------------------------*
      *    MODULOS.....:                                               *
      *    SCCE9100 - VERIFICA SE C/C ENCERRADA OU NAO RECADASTRADA    *
      *    MESU9015 - DEVOLVE OS DADOS DO BANCO                        *
      *    MESU9018 - DEVOLVE INFORMACOES DE UM PAB                    *
      *    NTAX5000 - REPASSA O CONTROLE PARA NTAX5001                 *
      *    GFCT5005 - OBTER DADOS DE TARIFA                            *
      *    GFCT5318 - OBTER PERCENTUAL TARIFA                          *
      *    POOL0536 - RETORNA AS INFORMACOES SOBRE O BANCO             *
      *    POOL7100 - CANCELAMENTO DO PROGRAMA                         *
      *    POOL7600 - OBTEM DATA E HORA DO SISTEMA.                    *
      *    SARH8000 - PERMITIR AOS SISTEMAS LEGADOS ACESSO AS INFORMA- *
      *               COES DE FUNCIONARIOS NO REPOSITORIO DE DADOS SAP *
      *================================================================*
      *================================================================*
      *    PROGRAMA....:   ALTERACAO                                   *
      *    ANALISTA....:   RODRIGO PRADO           - PROCKWORK GP. 50  *
      *    DATA........:   16/04/2007                                  *
      *----------------------------------------------------------------*
      *    OBJETIVO....:                                               *
      *           - NAO REJEITAR EVENTOS COM PAB INVALIDO              *
      *           - OBTER PAB E FORMATAR NO ARQUIVO OK PARA CARGA      *
      *================================================================*

      *================================================================*
      *                     A L T E R A C A O                          *
      *----------------------------------------------------------------*
      *                                                                *
      *    PROGRAMADOR  :  RICARDO P. DA SILVA           -  PROCWORK   *
      *    ANALISTA     :  RICARDO P. DA SILVA           -  PROCWORK   *
      *    DATA         :  10/08/2009                                  *
      *                                                                *
      *    OBJETIVO     :                                              *
      *      PROCESSAR A CONSISTENCIA LOGICA PARA O NOVO TIPO DE       *
      *      REGISTRO = 4.                                             *
      *                                                                *
      *================================================================*

      *================================================================*
      *                     A L T E R A C A O                          *
      *----------------------------------------------------------------*
      *                                                                *
      *    PROGRAMADOR  :  RODRIGO PRADO        - SONDA PROCWORK       *
      *    ANALISTA     :  RODRIGO PRADO        - SONDA PROCWORK       *
      *    DATA         :  07/11/2009                                  *
      *                                                                *
      *    OBJETIVO     :                                              *
      *      CONSISTIR O ANO DE REFERENCIA DE EMISSAO DO EXTRATO       *
      *      ANUAL VIA BDN PARA TRATAMENTO DE FRANQUIA UNICA NA        *
      *      REGRA DE TARIFA ( TELA 2 CAD TARIFAS ) NA NET 1010.       *
      *                                                                *
      *================================================================*
      *
      *================================================================*
      *                     A L T E R A C A O                          *
      *----------------------------------------------------------------*
      *                                                                *
      *    PROGRAMADOR  :  CIBELE               - SONDA PROCWORK       *
      *    ANALISTA     :  HELENA               - DDS                  *
      *    DATA         :  25/11/2010                                  *
      *                                                                *
      *    OBJETIVO     :                                              *
      *      RECOMPILACAO A PEDIDO DO ANALISTA. *
      *                                                                *
      *================================================================*
      *===============================================================*
      *                   M A N U T E N C O E S                       *
      *===============================================================*
      *---------------------------------------------------------------*
      *    OBJETIVO....:   ALTERACAO DO TAMANHO DA COLUNA DA TABELA   *
      *     (JC0413)       GFCTBA2 - QDIA_CAREN_TARIF DE 9(02) PARA   *
      *                    9(03) -  OS MODULOS: GFCT5005 E GFCT5318   *
      *                    CHAMADOS PELO PROGRAMA  POSSUEM O CAMPO    *
      *                    QDIA-CAREN-TARIF NA AREA DE LINKAGE.       *
      *---------------------------------------------------------------*
      *    PROGRAMADOR.:   J.C.R.                    - SONDA IT       *
      *    ANALISTA DDS:   HELENA                    - GP.50          *
      *    ANALISTA....:   PAGNNOCA                  - SONDA IT       *
      *    DATA........:   ABRIL/2013                                 *
      *---------------------------------------------------------------*
      *===============================================================*
PR2801*================================================================*
'     *                        A L T E R A C A O                       *
'     *----------------------------------------------------------------*
'     * ANALISTA       :  PRISCILLA MARQUES - CAPGEMINI                *
'     * DATA           :  JAN/2016                                     *
'     * OBJETIVO       :  INCLUSAO DE COMMIT'S PERIODICOS              *
'     *                   (RS 3.083.908)                               *
'     *================================================================*
'     * PROGRAMA COM LEITURA DE BANCO DE DADOS E LIBERACAO DE RECURSOS *
'     * ============================================================== *
'     *                                                                *
'     * EMBORA NAO TENHA ATUALIZACAO DE BANCO DE DADOS, ESSE PROGRAMA  *
'     * SOFREU ATUALIZACAO PARA INCLUSAO DE COMMIT, VISANDO LIBERACAO  *
'     * PERIODICA DE RECURSOS, POIS O MESMO CONSTAVA  NA  RELACAO  DE  *
'     * PROGRAMAS "LONG RUNNING READERS" - FAZEM  APENAS  LEITURA  DE  *
'     * TABELAS DB2 E ATINGEM MAIS DE 30/60 MINUTOS DE  EXECUCAO  SEM  *
'     * COMMIT.                                                        *
'     *                                                                *
'     *                        A T E N C A O                           *
'     *                        = = = = = = =                           *
'     *                                                                *
'     * CASO ESSE PROGRAMA PASSE A TER INCLUSAO, ALTERACAO OU DELECAO  *
'     * EM BANCOS DE DADOS DB2, O COMMIT DEVERA OBRIGATORIAMENTE  SER  *
'     * SUBSTITUIDO PELO COMPONENTE CKRS-CHECKPOINT/RESTART, SOB PENA  *
'     * DE PROBLEMAS IRREVERSIVEIS EM CASO DE ABEND.                   *
PR2801*================================================================*
AT0522* ALTERACOES:
AT0522*================================================================
AT0522* DATA       NOME          DESCRICAO
AT0522*========== ============   ======================================
AT0522*18/05/2022 ANGELO TROMBIM - ALTERACAO NO ACESSO A TABELA
AT0522*                            ANPRV005 PARA O SERVICO CARO8200.
AT0522*                          - AJUSTE DAS VARIAVEIS REFERENTE A
AT0522*                            TABELA ANPRV005 PARA CARO8200.
      *================================================================*
      *---------------------------------------------------------------*
      *            U L T I M A    A L T E R A C A O                   *
      *---------------------------------------------------------------*
      *   ANALISTA     -  FABIO AIRES                  - CAPGEMINI    *
      *   ANALISTA     -  ANDRE DE SILVA LIMA          - GP408        *
      *   DATA         -  JULHO/2024                                  *
      *---------------------------------------------------------------*
      *                         OBJETIVO                              *
      *---------------------------------------------------------------*
      *   DEVIDO ALTO CONSUMO NO JOB CMTFC545 FOI CRIADO UM PARM PARA *
      *   TRATAMENTO DIFERENCIADO  A FIM DE OBTER GANHOS DE PERFORMAN-*
      *   CE. SE O PARM=PERF TERA TRATAMENTO O ARQUIVO GFCTMVTE QUE   *
      *   FOI GERADO PELO PROGRAMA GFCT1104 E CONTEM INFORMACOES DAS  *
      *   TABELAS CLIEV004, CLIEV007 E CLIEV008                       *
      *   RITM0402786/RITM0713598                                     *
      *---------------------------------------------------------------*


           EJECT

      *================================================================*
       ENVIRONMENT                     DIVISION.
      *================================================================*

      *----------------------------------------------------------------*
       CONFIGURATION                   SECTION.
      *----------------------------------------------------------------*

       SPECIAL-NAMES.
           DECIMAL-POINT               IS COMMA.

      *----------------------------------------------------------------*
       INPUT-OUTPUT                    SECTION.
      *----------------------------------------------------------------*

       FILE-CONTROL.

           SELECT GFCTSEQE ASSIGN      TO UT-S-GFCTSEQE
           FILE STATUS                 IS WRK-FS-GFCTSEQE.

           SELECT GFCTMVTA ASSIGN      TO UT-S-GFCTMVTA
           FILE STATUS                 IS WRK-FS-GFCTMVTA.

FAIRES     SELECT GFCTMVTE ASSIGN      TO UT-S-GFCTMVTE
FAIRES     FILE STATUS                 IS WRK-FS-GFCTMVTE.


           SELECT GFCTMVOK ASSIGN      TO UT-S-GFCTMVOK
           FILE STATUS                 IS WRK-FS-GFCTMVOK.

           SELECT GFCTMVRJ ASSIGN      TO UT-S-GFCTMVRJ
           FILE STATUS                 IS WRK-FS-GFCTMVRJ.

           SELECT GFCTMVER ASSIGN      TO UT-S-GFCTMVER
           FILE STATUS                 IS WRK-FS-GFCTMVER.

           SELECT GFCTSEQS ASSIGN      TO UT-S-GFCTSEQS
           FILE STATUS                 IS WRK-FS-GFCTSEQS.

           EJECT
      *================================================================*
       DATA                            DIVISION.
      *================================================================*

      *----------------------------------------------------------------*
       FILE                            SECTION.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *    INPUT:  ARQUIVO CONTROLE SEQUENCIA MOVTO TARIFAS PARA GFCT  *
      *            ORG. SEQUENCIAL     -   LRECL   =   150             *
      *----------------------------------------------------------------*

       FD  GFCTSEQE
           RECORDING MODE IS F
           LABEL RECORD IS STANDARD
           BLOCK CONTAINS 0 RECORDS.

       COPY 'I#GFCT05'.

      *----------------------------------------------------------------*
      *    INPUT:  ARQUIVO INTERFACE PADRAO MOVIMENTO TARIFAS          *
      *            ORG. SEQUENCIAL     -   LRECL   =   300             *
      *----------------------------------------------------------------*

       FD  GFCTMVTA
           RECORDING    MODE  IS  F
           LABEL RECORD IS STANDARD
           BLOCK CONTAINS 0 RECORDS.

       COPY 'GFCTWBD2'.

FAIRES*----------------------------------------------------------------*
  "   *    INPUT:  ARQUIVO INTERFACE PADRAO MOVIMENTO TARIFAS          *
  "   *            ORG. SEQUENCIAL     -   LRECL   =   349             *
  "   *----------------------------------------------------------------*
  "
  "    FD  GFCTMVTE
  "        RECORDING    MODE  IS  F
  "        LABEL RECORD IS STANDARD
  "        BLOCK CONTAINS 0 RECORDS.
  "
  "    01 FD-REG-GFCTMVTE            PIC X(349).
FAIRES

      *----------------------------------------------------------------*
      *    OUTPUT: ARQUIVO INTERFACE PADRAO MOVIMENTO TARIFAS OK       *
      *            ORG. SEQUENCIAL     -   LRECL   =   300             *
      *----------------------------------------------------------------*

       FD  GFCTMVOK
           RECORDING MODE IS F
           LABEL RECORD IS STANDARD
           BLOCK CONTAINS 0 RECORDS.

       COPY 'GFCTWBD2'.

      *----------------------------------------------------------------*
      *    OUTPUT: ARQUIVO INTERFACE PADRAO MOVTO TARIFAS REJEITADOS   *
      *            ORG. SEQUENCIAL     -   LRECL   =   300             *
      *----------------------------------------------------------------*

       FD  GFCTMVRJ
           RECORDING MODE IS F
           LABEL RECORD IS STANDARD
           BLOCK CONTAINS 0 RECORDS.

       COPY 'GFCTWBD2'.

      *----------------------------------------------------------------*
      *    OUTPUT: MOVIMENTO TARIFAS - ERROS                           *
      *            ORG. SEQUENCIAL     -   LRECL   =   300             *
      *----------------------------------------------------------------*

       FD  GFCTMVER
           RECORDING MODE IS F
           LABEL RECORD IS STANDARD
           BLOCK CONTAINS 0 RECORDS.

       COPY 'GFCTWBD3'.

      *----------------------------------------------------------------*
      *   OUTPUT:  ARQUIVO CONTROLE SEQUENCIA MOVTO TARIFAS PARA GFCT  *
      *            ORG. SEQUENCIAL     -   LRECL   =   150             *
      *----------------------------------------------------------------*

       FD  GFCTSEQS
           RECORDING MODE IS F
           LABEL RECORD IS STANDARD
           BLOCK CONTAINS 0 RECORDS.

       COPY 'I#GFCT05'.

           EJECT
      *----------------------------------------------------------------*
       WORKING-STORAGE                 SECTION.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       77  FILLER                      PIC  X(050)         VALUE
           '*** GFCT1105 - INICIO DA AREA DE WORKING ***'.
      *----------------------------------------------------------------*

      *---------------------------------------------------------------*
       77  FILLER                      PIC  X(050)         VALUE
           '*** AREA PARA INDEXADOR ***'.
      *---------------------------------------------------------------*

       77  IND-1                       PIC  9(003)         VALUE ZEROS.
       77  IND-2                       PIC  9(003)         VALUE ZEROS.

           EJECT
      *---------------------------------------------------------------*
       77  FILLER                      PIC  X(050)         VALUE
           '*** AREAS AUXILIARES ***'.
      *---------------------------------------------------------------*

       01  WRK-CAMPO-TIRA-SINAL        PIC +9(017)         VALUE ZEROS.
       01  FILLER  REDEFINES           WRK-CAMPO-TIRA-SINAL.
           05  WRK-CAMPO-TIRA-SINAL-S  PIC X.
           05  WRK-CAMPO-TIRA-SINAL-17 PIC 9(17).
           05  FILLER REDEFINES WRK-CAMPO-TIRA-SINAL-17.
               10 FILLER                  PIC X(01).
               10 WRK-CAMPO-TIRA-SINAL-16 PIC 9(16).
           05  FILLER REDEFINES WRK-CAMPO-TIRA-SINAL-17.
               10 FILLER                  PIC X(02).
               10 WRK-CAMPO-TIRA-SINAL-15 PIC 9(15).
           05  FILLER REDEFINES WRK-CAMPO-TIRA-SINAL-17.
               10 FILLER                  PIC X(03).
               10 WRK-CAMPO-TIRA-SINAL-14 PIC 9(14).
           05  FILLER REDEFINES WRK-CAMPO-TIRA-SINAL-17.
               10 FILLER                  PIC X(04).
               10 WRK-CAMPO-TIRA-SINAL-13 PIC 9(13).
           05  FILLER REDEFINES WRK-CAMPO-TIRA-SINAL-17.
               10 FILLER                  PIC X(05).
               10 WRK-CAMPO-TIRA-SINAL-12 PIC 9(12).
           05  FILLER REDEFINES WRK-CAMPO-TIRA-SINAL-17.
               10 FILLER                  PIC X(06).
               10 WRK-CAMPO-TIRA-SINAL-11 PIC 9(11).
           05  FILLER REDEFINES WRK-CAMPO-TIRA-SINAL-17.
               10 FILLER                  PIC X(07).
               10 WRK-CAMPO-TIRA-SINAL-10 PIC 9(10).
           05  FILLER REDEFINES WRK-CAMPO-TIRA-SINAL-17.
               10 FILLER                  PIC X(08).
               10 WRK-CAMPO-TIRA-SINAL-09 PIC 9(09).
           05  FILLER REDEFINES WRK-CAMPO-TIRA-SINAL-17.
               10 FILLER                  PIC X(09).
               10 WRK-CAMPO-TIRA-SINAL-08 PIC 9(08).
           05  FILLER REDEFINES WRK-CAMPO-TIRA-SINAL-17.
               10 FILLER                  PIC X(10).
               10 WRK-CAMPO-TIRA-SINAL-07 PIC 9(07).
           05  FILLER REDEFINES WRK-CAMPO-TIRA-SINAL-17.
               10 FILLER                  PIC X(11).
               10 WRK-CAMPO-TIRA-SINAL-06 PIC 9(06).
           05  FILLER REDEFINES WRK-CAMPO-TIRA-SINAL-17.
               10 FILLER                  PIC X(12).
               10 WRK-CAMPO-TIRA-SINAL-05 PIC 9(05).
           05  FILLER REDEFINES WRK-CAMPO-TIRA-SINAL-17.
               10 FILLER                  PIC X(13).
               10 WRK-CAMPO-TIRA-SINAL-04 PIC 9(04).
           05  FILLER REDEFINES WRK-CAMPO-TIRA-SINAL-17.
               10 FILLER                  PIC X(14).
               10 WRK-CAMPO-TIRA-SINAL-03 PIC 9(03).
           05  FILLER REDEFINES WRK-CAMPO-TIRA-SINAL-17.
               10 FILLER                  PIC X(15).
               10 WRK-CAMPO-TIRA-SINAL-02 PIC 9(02).
           05  FILLER REDEFINES WRK-CAMPO-TIRA-SINAL-17.
               10 FILLER                  PIC X(16).
               10 WRK-CAMPO-TIRA-SINAL-01 PIC 9(01).

       01  WRK-CAMPO-POE-SINAL-01        PIC +9(01)      VALUE ZEROS.
       01  FILLER  REDEFINES             WRK-CAMPO-POE-SINAL-01.
           05  WRK-CAMPO-POE-SINAL-01-S  PIC X.
           05  WRK-CAMPO-POE-SINAL-01-C  PIC 9(01).

       01  WRK-CAMPO-POE-SINAL-02        PIC +9(02)      VALUE ZEROS.
       01  FILLER  REDEFINES             WRK-CAMPO-POE-SINAL-02.
           05  WRK-CAMPO-POE-SINAL-02-S  PIC X.
           05  WRK-CAMPO-POE-SINAL-02-C  PIC 9(02).

       01  WRK-CAMPO-POE-SINAL-03        PIC +9(03)      VALUE ZEROS.
       01  FILLER  REDEFINES             WRK-CAMPO-POE-SINAL-03.
           05  WRK-CAMPO-POE-SINAL-03-S  PIC X.
           05  WRK-CAMPO-POE-SINAL-03-C  PIC 9(03).

       01  WRK-CAMPO-POE-SINAL-04        PIC +9(04)      VALUE ZEROS.
       01  FILLER  REDEFINES             WRK-CAMPO-POE-SINAL-04.
           05  WRK-CAMPO-POE-SINAL-04-S  PIC X.
           05  WRK-CAMPO-POE-SINAL-04-C  PIC 9(04).

       01  WRK-CAMPO-POE-SINAL-05        PIC +9(05)      VALUE ZEROS.
       01  FILLER  REDEFINES             WRK-CAMPO-POE-SINAL-05.
           05  WRK-CAMPO-POE-SINAL-05-S  PIC X.
           05  WRK-CAMPO-POE-SINAL-05-C  PIC 9(05).

       01  WRK-CAMPO-POE-SINAL-06        PIC +9(06)      VALUE ZEROS.
       01  FILLER  REDEFINES             WRK-CAMPO-POE-SINAL-06.
           05  WRK-CAMPO-POE-SINAL-06-S  PIC X.
           05  WRK-CAMPO-POE-SINAL-06-C  PIC 9(06).

       01  WRK-CAMPO-POE-SINAL-07        PIC +9(07)      VALUE ZEROS.
       01  FILLER  REDEFINES             WRK-CAMPO-POE-SINAL-07.
           05  WRK-CAMPO-POE-SINAL-07-S  PIC X.
           05  WRK-CAMPO-POE-SINAL-07-C  PIC 9(07).

       01  WRK-CAMPO-POE-SINAL-08        PIC +9(08)      VALUE ZEROS.
       01  FILLER  REDEFINES             WRK-CAMPO-POE-SINAL-08.
           05  WRK-CAMPO-POE-SINAL-08-S  PIC X.
           05  WRK-CAMPO-POE-SINAL-08-C  PIC 9(08).

       01  WRK-CAMPO-POE-SINAL-09        PIC +9(09)      VALUE ZEROS.
       01  FILLER  REDEFINES             WRK-CAMPO-POE-SINAL-09.
           05  WRK-CAMPO-POE-SINAL-09-S  PIC X.
           05  WRK-CAMPO-POE-SINAL-09-C  PIC 9(09).

       01  WRK-CAMPO-POE-SINAL-10        PIC +9(10)      VALUE ZEROS.
       01  FILLER  REDEFINES             WRK-CAMPO-POE-SINAL-10.
           05  WRK-CAMPO-POE-SINAL-10-S  PIC X.
           05  WRK-CAMPO-POE-SINAL-10-C  PIC 9(10).

       01  WRK-CAMPO-POE-SINAL-11        PIC +9(11)      VALUE ZEROS.
       01  FILLER  REDEFINES             WRK-CAMPO-POE-SINAL-11.
           05  WRK-CAMPO-POE-SINAL-11-S  PIC X.
           05  WRK-CAMPO-POE-SINAL-11-C  PIC 9(11).

       01  WRK-CAMPO-POE-SINAL-12        PIC +9(12)      VALUE ZEROS.
       01  FILLER  REDEFINES             WRK-CAMPO-POE-SINAL-12.
           05  WRK-CAMPO-POE-SINAL-12-S  PIC X.
           05  WRK-CAMPO-POE-SINAL-12-C  PIC 9(12).

       01  WRK-CAMPO-POE-SINAL-13        PIC +9(13)      VALUE ZEROS.
       01  FILLER  REDEFINES             WRK-CAMPO-POE-SINAL-13.
           05  WRK-CAMPO-POE-SINAL-13-S  PIC X.
           05  WRK-CAMPO-POE-SINAL-13-C  PIC 9(13).

       01  WRK-CAMPO-POE-SINAL-14        PIC +9(14)      VALUE ZEROS.
       01  FILLER  REDEFINES             WRK-CAMPO-POE-SINAL-14.
           05  WRK-CAMPO-POE-SINAL-14-S  PIC X.
           05  WRK-CAMPO-POE-SINAL-14-C  PIC 9(14).

       01  WRK-CAMPO-POE-SINAL-15        PIC +9(15)      VALUE ZEROS.
       01  FILLER  REDEFINES             WRK-CAMPO-POE-SINAL-15.
           05  WRK-CAMPO-POE-SINAL-15-S  PIC X.
           05  WRK-CAMPO-POE-SINAL-15-C  PIC 9(15).

       01  WRK-CAMPO-POE-SINAL-16        PIC +9(16)      VALUE ZEROS.
       01  FILLER  REDEFINES             WRK-CAMPO-POE-SINAL-16.
           05  WRK-CAMPO-POE-SINAL-16-S  PIC X.
           05  WRK-CAMPO-POE-SINAL-16-C  PIC 9(16).

       01  WRK-CAMPO-POE-SINAL-17        PIC +9(17)      VALUE ZEROS.
       01  FILLER  REDEFINES             WRK-CAMPO-POE-SINAL-17.
           05  WRK-CAMPO-POE-SINAL-17-S  PIC X.
           05  WRK-CAMPO-POE-SINAL-17-C  PIC 9(17).


       77  WRK-IO-PCB                          PIC  X(032) VALUE SPACES.
       77  WRK-ALT-PCB                         PIC  X(032) VALUE SPACES.
       77  WRK-MASCARA                         PIC  ZZ.ZZZ.ZZ9 VALUE
           ZERO.
       77  WRK-MODULO                          PIC  X(008) VALUE SPACES.
       77  WRK-BATCH                           PIC  X(008) VALUE
           'BATCH'.

       77 WRK-DATA                             PIC  X(010) VALUE SPACES.
       77 WRK-IND                              PIC  9(003) VALUE ZEROS.
       77 WRK-IND1                             PIC  9(003) VALUE ZEROS.
       77 WRK-IND-ERRO-TPCTA                   PIC  X(001) VALUE 'N'.
       77 WRK-IND-TAR-MOEDA-EVENTO             PIC  X(001) VALUE 'N'.
       77 WRK-FINALIZAR                        PIC  X(001) VALUE 'N'.
       77 WRK-TIPO-CONTA                       PIC  X(001) VALUE 'N'.

PR2801 77 ACU-COMMIT                    PIC  9(09) COMP-3 VALUE ZEROS.

       77 WRK-CGRP-CTBIL-ATUAL                PIC S9(3)V COMP-3 VALUE 0.
       77 WRK-CSGRP-CTBIL                     PIC S9(3)V COMP-3 VALUE 0.


       77  WRK-POSTO-SERVC                     PIC  9(005) VALUE ZEROS.
       77  WRK-TP-REG-ANT                      PIC  X(001) VALUE ZEROS.
       77  WRK-EMPR                            PIC  9(003) VALUE ZEROS.
       77  WRK-DEPDC                           PIC  9(005) VALUE ZEROS.
       77  WRK-CORR-BANCARIO                   PIC  X(015) VALUE SPACES.
       77  WRK-COMU-COD-EMPR                   PIC  9(005) VALUE ZEROS.
       77  WRK-COD-DEPDC                       PIC  9(005) VALUE ZEROS.
       77  WRK-COD-POSTO                       PIC  9(005) VALUE ZEROS.

       77  WRK-IND-ERRO-FIS                    PIC  X(001) VALUE SPACES.
       77  WRK-IND-ERRO-LOG                    PIC  X(001) VALUE SPACES.

       77  WRK-AUX-COD-TARIFA                  PIC  X(005) VALUE SPACES.
       77  WRK-BANCO                           PIC  X(005) VALUE SPACES.
       77  WRK-COD-MENSAGEM                    PIC  9(005) VALUE ZEROS.
       77  WRK-CROTNA-ORIGE                    PIC  X(005) VALUE SPACES.
       77  WRK-CT-DEVOLVIDO                    PIC  9(008) VALUE ZEROS.
FAIRES 77  WRK-ACU-NACHOU-CLIEV004             PIC  9(008) VALUE ZEROS.
FAIRES 77  WRK-ACU-NACHOU-CLIEV007             PIC  9(008) VALUE ZEROS.
FAIRES 77  WRK-ACU-NACHOU-CLIEV008             PIC  9(008) VALUE ZEROS.
FAIRES 77  WRK-ACU-ACHOU-CLIEV004              PIC  9(008) VALUE ZEROS.
FAIRES 77  WRK-ACU-ACHOU-CLIEV007              PIC  9(008) VALUE ZEROS.
FAIRES 77  WRK-ACU-ACHOU-CLIEV008              PIC  9(008) VALUE ZEROS.
       77  WRK-CT-GRAV-ERR                     PIC  9(008) VALUE ZEROS.
       77  WRK-CT-GRAV-OK                      PIC  9(008) VALUE ZEROS.
       77  WRK-CT-GRAV-SEQ                     PIC  9(008) VALUE ZEROS.
       77  WRK-CT-EVN-RECBD                    PIC  9(008) VALUE ZEROS.
       77  WRK-CT-EVN-OK                       PIC  9(008) VALUE ZEROS.
       77  WRK-CT-GRV-OK                       PIC  9(008) VALUE ZEROS.
       77  WRK-CT-GRV-OK-0                     PIC  9(008) VALUE ZEROS.
       77  WRK-CT-GRV-OK-1                     PIC  9(008) VALUE ZEROS.
       77  WRK-CT-GRV-OK-2                     PIC  9(008) VALUE ZEROS.
       77  WRK-CT-GRV-OK-3                     PIC  9(008) VALUE ZEROS.
       77  WRK-CT-GRV-OK-4               PIC  9(008) COMP-3 VALUE ZEROS.
       77  WRK-CTM                       PIC  9(005) COMP-3 VALUE ZEROS.
       77  WRK-CTN                       PIC  9(005) COMP-3 VALUE ZEROS.
       77  WRK-ACHOU                     PIC  X(001)        VALUE 'N'.
______*77__WRK-PESQ-CORR_________________PIC__X(001)________VALUE_'N'.
       77  WRK-CT-GRV-OK-9                     PIC  9(008) VALUE ZEROS.
       77  WRK-CT-GRV-REJ                      PIC  9(008) VALUE ZEROS.
       77  WRK-CT-GRV-REJ-0                    PIC  9(008) VALUE ZEROS.
       77  WRK-CT-GRV-REJ-1                    PIC  9(008) VALUE ZEROS.
       77  WRK-CT-GRV-REJ-2                    PIC  9(008) VALUE ZEROS.
       77  WRK-CT-GRV-REJ-3                    PIC  9(008) VALUE ZEROS.
       77  WRK-CT-GRV-REJ-4              PIC  9(008) COMP-3 VALUE ZEROS.
       77  WRK-CT-GRV-REJ-9                    PIC  9(008) VALUE ZEROS.
       77  WRK-CT-LIDOS                        PIC  9(008) VALUE ZEROS.
       77  WRK-CT-LIDOS-0                      PIC  9(008) VALUE ZEROS.
       77  WRK-CT-LIDOS-1                      PIC  9(008) VALUE ZEROS.
       77  WRK-CT-LIDOS-2                      PIC  9(008) VALUE ZEROS.
       77  WRK-CT-LIDOS-3                      PIC  9(008) VALUE ZEROS.
       77  WRK-CT-LIDOS-4                PIC  9(008) COMP-3 VALUE ZEROS.
       77  WRK-CT-LIDOS-9                      PIC  9(008) VALUE ZEROS.
       77  WRK-CT-LIDOS-SEQ                    PIC  9(008) VALUE ZEROS.
       77  WRK-DATA-MOVTO                      PIC  X(010) VALUE SPACES.
       77  WRK-EVN-AGE-ORIG                    PIC  X(005) VALUE SPACES.
       77  WRK-EVN-AGE-DEST                    PIC  X(005) VALUE SPACES.
       77  WRK-EVN-COD-TARIFA                  PIC  9(005) VALUE ZEROS.
       77  WRK-EVN-COD-TARIFA-CORR             PIC  9(005) VALUE ZEROS.
       77  WRK-EVN-CTA-DEST                    PIC  X(013) VALUE SPACES.
       77  WRK-EVN-PAB-ORIG                    PIC  X(005) VALUE SPACES.
       77  WRK-EVN-PAB-DEST                    PIC  X(005) VALUE SPACES.
       77  WRK-IDENT-CLIE                      PIC  X(030) VALUE SPACES.
       77  WRK-INDARQ-REJ                      PIC  X(001) VALUE SPACES.
       77  WRK-IND-ERRO-DETALHE                PIC  X(001) VALUE SPACES.
       77  WRK-IND-ERRO-DETALHE-CLIE           PIC  X(001) VALUE SPACES.
       77  WRK-IND-ERRO-MIDIA                  PIC  X(001) VALUE SPACES.
       77  WRK-IND-ERRO-PERIF                  PIC  X(001) VALUE SPACES.
       77  WRK-IND-ERRO-GRUPO                  PIC  X(001) VALUE SPACES.
       77  WRK-IND-ERRO-HEADER                 PIC  X(001) VALUE SPACES.
       77  WRK-IND-FIM-ARQ-MOV                 PIC  X(001) VALUE SPACES.
       77  WRK-IND-FIM-ARQ-SEQ                 PIC  X(001) VALUE SPACES.
       77  WRK-IND-MOVTO-OK                    PIC  X(001) VALUE SPACES.
       77  WRK-INDREG-REJ                      PIC  X(001) VALUE SPACES.
       77  WRK-SEQ-ARQ                         PIC  9(011) VALUE ZEROS.
       77  WRK-TRATAMENTO                      PIC  9(002) VALUE ZEROS.
       77  WRK-PERC-DEVOLU                     PIC  9(003) VALUE ZEROS.
       77  WRK-PERC-DEVOLVI                    PIC  9(003)V99
                                                           VALUE ZEROS.
       77  WRK-CLIE-CRTT                       PIC  X(001) VALUE SPACES.
       77  WRK-ROT-ORIG                        PIC  X(005) VALUE SPACES.
       77  WRK-TEM-PERCENTUAL                  PIC  X(001) VALUE SPACES.
       77  WRK-EVNTO-PARC                      PIC  X(001) VALUE SPACES.
       77  WRK-EVN-VALOR-PRINC                 PIC  9(16)V99 VALUE ZERO.
       77  WRK-EVN-VALOR-TOT-SERVC             PIC  9(16)V99 VALUE ZERO.
       77  WRK-CID-CLI                         PIC  X(026) VALUE SPACES.
       77  WRK-CTPO-PSSOA                      PIC  X(001) VALUE SPACES.
       77  WRK-TIPO-PSSOA-CLIE                 PIC  X(001) VALUE SPACES.
       77  WRK-S5-5-S                          PIC S9(005)V VALUE ZEROS.

       77  FILLER                              PIC  X(300) VALUE
                                                           LOW-VALUES.

FAIRES 01  WRK-REG-GFCTMVTE.
  "        05 WRK-PRIPARTE-GFCTMVTE    PIC  X(300)       VALUE SPACES.
  "        05 WRK-SEGPARTE-GFCTMVTE    PIC  9(010)       VALUE ZEROS.
  "        05 WRK-POSTO-SERVC-E        PIC  9(05)        VALUE ZEROS.
  "        05 WRK-CID-CLI-E            PIC  X(26)        VALUE SPACES.
  "        05 WRK-CGRP-CTBIL-ATUAL-E   PIC S9(3)V COMP-3 VALUE ZEROS.
  "        05 WRK-CSGRP-CTBIL-E        PIC S9(3)V COMP-3 VALUE ZEROS.
  "        05 WRK-TIPO-PSSOA-E         PIC  X(01)        VALUE SPACES.
  "        05 WRK-INDIC-CLIEV004       PIC  X(01)        VALUE SPACES.
  "        05 WRK-INDIC-CLIEV007       PIC  X(01)        VALUE SPACES.
FAIRES     05 WRK-INDIC-CLIEV008       PIC  X(01)        VALUE SPACES.

FAIRES 01  WRK-CODIGO-TARIF-ANT        PIC  9(005)       VALUE ZEROS.
FAIRES 01  WRK-DT-INI-VIGEN-ANT        PIC  X(010)       VALUE SPACES.
FAIRES 01  WRK-CODIGO-TARIF-ANT2       PIC  9(005)       VALUE ZEROS.
FAIRES 01  WRK-DT-INI-VIGEN-ANT2       PIC  X(010)       VALUE SPACES.

       01  WRK-AUX-COD-TARIFA-CORR.
           05  WRK-AUX-COD-TARIFA-CORR-R       PIC  9(005) VALUE ZEROS.

       01  WRK-DISP-CORR-AST-1              PIC X(50) VALUE
           '=================================================='.

       01  WRK-DISP-CORR-AST-2              PIC X(50) VALUE
           '======== CONVERSAO DE TARIFAS CORRELATAS ========='.

       01  WRK-DISP-CORR-AST-3              PIC X(50) VALUE
           '=================================================='.

       01  WRK-DISP-CORR-AST-4.
           05  FILLER                       PIC X(5)  VALUE
               'IDX'.
           05  FILLER                       PIC X(03) VALUE ' - '.
           05  FILLER                       PIC X(04) VALUE
               'ROT'.
           05  FILLER                       PIC X(03) VALUE ' - '.
           05  FILLER                       PIC X(06) VALUE
               'CORREL'.
           05  FILLER                       PIC X(03) VALUE ' - '.
           05  FILLER                       PIC X(06) VALUE
               'TARIFA'.
           05  FILLER                       PIC X(03) VALUE ' - '.
           05  FILLER                       PIC X(02) VALUE
               'TP'.
           05  FILLER                       PIC X(03) VALUE ' - '.
           05  FILLER                       PIC X(10) VALUE
               'CONVERSOES'.


       01  WRK-DISP-CORR-AST-5.
           05  WRK-D-CORR-IDX               PIC 99999 VALUE ZEROS.
           05  FILLER                       PIC X(03) VALUE ' - '.
           05  WRK-D-CSIST-CORRL            PIC X(04) VALUE ' '.
           05  FILLER                       PIC X(03) VALUE ' - '.
           05  WRK-D-CERVC-TARIF-CORRL      PIC +9(5) VALUE ZEROS.
           05  FILLER                       PIC X(03) VALUE ' - '.
           05  WRK-D-CSERVC-TARIF           PIC +9(5) VALUE ZEROS.
           05  FILLER                       PIC X(03) VALUE ' - '.
           05  WRK-D-CTPO-SERVC-TARIF       PIC +9(1) VALUE ZEROS.
           05  FILLER                       PIC X(03) VALUE ' - '.
           05  WRK-D-QTDE-CONV              PIC +9(9) VALUE ZEROS.

       COPY 'GFCTWBD1'.

       01  WRK-S5-5                    PIC +9(005)         VALUE ZEROS.
       01  FILLER                      REDEFINES           WRK-S5-5.
           05  WRK-S5-5-SINAL          PIC  X(001).
           05  WRK-S5-5-R              PIC  9(005).

       01  WRK-CCUSTO-X.
           05 WRK-CCUSTO                       PIC  X(004)  VALUE SPACE.
           05 FILLER                           PIC  X(001)  VALUE SPACE.

       01  WRK-TAB-GRUPO.
           03  WRK-TBGRP               OCCURS 500 TIMES.
               05  WRK-TBGRP-REG.
                   07  WRK-TBGRP-SEQ-ARQ       PIC  X(011) VALUE SPACES.
                   07  WRK-TBGRP-CHAVE.
                       09  WRK-TBGRP-TP-REG    PIC  X(001) VALUE SPACES.
                       09  WRK-TBGRP-SQ-REG    PIC  X(011) VALUE SPACES.
                       09  WRK-TBGRP-SQ-CTRL   PIC  X(009) VALUE SPACES.
                       09  WRK-TBGRP-SQ-GRPE   PIC  X(009) VALUE SPACES.
                   07  FILLER                  PIC  X(257) VALUE SPACES.
                   07  WRK-TBGRP-ERRO-FIS      PIC  X(001) VALUE SPACES.
                   07  WRK-TBGRP-ERRO-LOG      PIC  X(001) VALUE SPACES.
               05  WRK-TBGRP-COD-MSG           PIC  9(005) VALUE ZEROS.

FAIRES*----------------------------------------------------------------*
  "   *       AREA PARA  ZERAR TABELA INTERNA POR EXPANSAO             *
  "   *----------------------------------------------------------------*
  "    01  WRK-TBGRP-I.
  "        05  WRK-TBGRP-REG-I.
  "            07  WRK-TBGRP-SEQ-ARQ-I         PIC  X(011) VALUE SPACES.
  "            07  WRK-TBGRP-CHAVE-I.
  "                09  WRK-TBGRP-TP-REG-I      PIC  X(001) VALUE SPACES.
  "                09  WRK-TBGRP-SQ-REG-I      PIC  X(011) VALUE SPACES.
  "                09  WRK-TBGRP-SQ-CTRL-I     PIC  X(009) VALUE SPACES.
  "                09  WRK-TBGRP-SQ-GRPE-I     PIC  X(009) VALUE SPACES.
  "            07  FILLER                      PIC  X(257) VALUE SPACES.
  "            07  WRK-TBGRP-ERRO-FIS-I        PIC  X(001) VALUE SPACES.
  "            07  WRK-TBGRP-ERRO-LOG-I        PIC  X(001) VALUE SPACES.
FAIRES     05  WRK-TBGRP-COD-MSG-I             PIC  9(005) VALUE ZEROS.

AT0522 01  WRK-CCTRO-CUSTO                     PIC  X(005) VALUE SPACES.
AT0522 01  FILLER REDEFINES WRK-CCTRO-CUSTO.
AT0522     05  WRK-CCTRO-CUSTO-R               PIC  X(004).
AT0522     05  FILLER                          PIC  X(001).

           EJECT
      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           '*** AREA PARA VARIAVEIS DE DATA ***'.
      *----------------------------------------------------------------*

       01  WRK-DATA-EVN-AUX.
           03  WRK-DIA-EVN             PIC  X(002)         VALUE SPACES.
           03  WRK-PT1                 PIC  X(001)         VALUE SPACES.
           03  WRK-MES-EVN             PIC  X(002)         VALUE SPACES.
           03  WRK-PT2                 PIC  X(001)         VALUE SPACES.
           03  WRK-ANO-EVN             PIC  X(004)         VALUE SPACES.

       01  WRK-DATA-VIGENCIA.
           03  WRK-DIA-VIG             PIC  X(002)         VALUE SPACES.
           03  FILLER                  PIC  X(001)         VALUE SPACES.
           03  WRK-MES-VIG             PIC  X(002)         VALUE SPACES.
           03  FILLER                  PIC  X(001)         VALUE SPACES.
           03  WRK-ANO-VIG             PIC  X(004)         VALUE SPACES.

       01  WRK-DATA-AUX.
           03  WRK-DIA-AUX             PIC  9(002)         VALUE ZEROS.
           03  FILLER                  PIC  X(001)         VALUE SPACES.
           03  WRK-MES-AUX             PIC  9(002)         VALUE ZEROS.
           03  FILLER                  PIC  X(001)         VALUE SPACES.
           03  WRK-ANO-AUX             PIC  9(004)         VALUE ZEROS.

       01  WRK-DATA-AUX2               PIC  9(009)         VALUE ZEROS.
       01  WRK-DATA-AUX2-R             REDEFINES WRK-DATA-AUX2.
           03  FILLER                  PIC  9(001).
           03  WRK-ANO-AUX2            PIC  9(004).
           03  WRK-MES-AUX2            PIC  9(002).
           03  WRK-DIA-AUX2            PIC  9(002).

       01  WRK-DATA-EVENTO.
           03  WRK-ANO-EVENTO          PIC  9(004)         VALUE ZEROS.
           03  WRK-MES-EVENTO          PIC  9(002)         VALUE ZEROS.
           03  WRK-DIA-EVENTO          PIC  9(002)         VALUE ZEROS.

       01  WRK-DPROCM-ATU.
           03  WRK-ANO-ATU             PIC  9(004)         VALUE ZEROS.
           03  WRK-MES-ATU             PIC  9(002)         VALUE ZEROS.
           03  WRK-DIA-ATU             PIC  9(002)         VALUE ZEROS.

       01  WRK-DPROCM-PROX.
           03  WRK-ANO-PROX            PIC  9(004)         VALUE ZEROS.
           03  WRK-MES-PROX            PIC  9(002)         VALUE ZEROS.
           03  WRK-DIA-PROX            PIC  9(002)         VALUE ZEROS.

       01  WRK-DATA-CONV-8.
           03  WRK-ANO-CONV8           PIC  9(004)         VALUE ZEROS.
           03  WRK-MES-CONV8           PIC  9(002)         VALUE ZEROS.
           03  WRK-DIA-CONV8           PIC  9(002)         VALUE ZEROS.

       01  WRK-DATA-DEBITO.
           03  WRK-DIA-DEB             PIC  9(002)         VALUE ZEROS.
           03  FILLER                  PIC  X(001)         VALUE ZEROS.
           03  WRK-MES-DEB             PIC  9(002)         VALUE ZEROS.
           03  FILLER                  PIC  X(001)         VALUE ZEROS.
           03  WRK-ANO-DEB             PIC  9(004)         VALUE ZEROS.

       01  WRK-DATA-DEB-8.
           03  WRK-ANO-DEB8            PIC  9(004)         VALUE ZEROS.
           03  WRK-MES-DEB8            PIC  9(002)         VALUE ZEROS.
           03  WRK-DIA-DEB8            PIC  9(002)         VALUE ZEROS.

       01  WRK-DATA-INI.
           03  WRK-ANO-INI             PIC  9(004)         VALUE ZEROS.
           03  WRK-MES-INI             PIC  9(002)         VALUE ZEROS.
           03  WRK-DIA-INI             PIC  9(002)         VALUE ZEROS.

       01  WRK-DATA-FIM.
           03  WRK-ANO-FIM             PIC  9(004)         VALUE ZEROS.
           03  WRK-MES-FIM             PIC  9(002)         VALUE ZEROS.
           03  WRK-DIA-FIM             PIC  9(002)         VALUE ZEROS.

       01  WRK-DATA-DB2.
           03  WRK-ANO-DB2             PIC  9(004)         VALUE ZEROS.
           03  FILLER                  PIC  X(001)         VALUE '-'.
           03  WRK-MES-DB2             PIC  9(002)         VALUE ZEROS.
           03  FILLER                  PIC  X(001)         VALUE '-'.
           03  WRK-DIA-DB2             PIC  9(002)         VALUE ZEROS.

       01  WRK-DATA1-AAAAMMDD          PIC  9(008) VALUE ZEROS.
       01  WRK-DATA1-AAAAMMDD-R        REDEFINES   WRK-DATA1-AAAAMMDD.
           03  WRK-DT1-AAAA            PIC  9(004).
           03  WRK-DT1-MM              PIC  9(002).
           03  WRK-DT1-DD              PIC  9(002).

       01  WRK-DATA2-DDMMAAAA.
           03  WRK-DT2-DD              PIC  X(002) VALUE ZEROS.
           03  FILLER                  PIC  X(001) VALUE '.'.
           03  WRK-DT2-MM              PIC  X(002) VALUE ZEROS.
           03  FILLER                  PIC  X(001) VALUE '.'.
           03  WRK-DT2-AAAA            PIC  X(004) VALUE ZEROS.

       01  WRK-HORA1-AUX               PIC  9(006) VALUE ZEROS.
       01  WRK-HORA1-AUX-R             REDEFINES   WRK-HORA1-AUX.
           03  WRK-HH1                 PIC  9(002).
           03  WRK-MM1                 PIC  9(002).
           03  WRK-SS1                 PIC  9(002).

       01  WRK-HORA2-AUX.
           03  WRK-HH2                 PIC  9(002) VALUE ZEROS.
           03  FILLER                  PIC  X(001) VALUE ':'.
           03  WRK-MM2                 PIC  9(002) VALUE ZEROS.
           03  FILLER                  PIC  X(001) VALUE ':'.
           03  WRK-SS2                 PIC  9(002) VALUE ZEROS.

       01  WRK-COD-FUNC2               PIC 9(009)        VALUE ZEROS.

       01  WRK-COD-FUNC                PIC 9(009)        VALUE ZEROS.
       01  WRK-COD-FUNC-R              REDEFINES WRK-COD-FUNC.
           05  FILLER                  PIC  X(002).
           05  WRK-COD-FUNC-X7         PIC  X(007).

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           '*** AREA MODULO SARH8000 ***'.
      *----------------------------------------------------------------*

       01  WRK-ENTRADA.
           03  WRK-PESQUISA-ENT        PIC  9(009)         VALUE ZEROS.
           03  WRK-TAMANHO-ENT         PIC  9(004)         VALUE ZEROS.
           03  WRK-SOLICITA-ENT.
               05  FILLER              OCCURS 255 TIMES.
                   07  WRK-CAMPO-ENT   PIC  9(003)         VALUE ZEROS.

       01  WRK-SAIDA.
           03  WRK-COD-RETORNO-SAI     PIC  9(002)         VALUE ZEROS.
           03  WRK-TAMANHO-SAI         PIC  9(004)         VALUE ZEROS.
           03  WRK-REGISTRO-SAI.
               05  FILLER              OCCURS 0 TO 1000 TIMES
                   DEPENDING ON WRK-TAMANHO-SAI.
                   07  WRK-CONTADOR-SAI PIC  X(001)        VALUE SPACES.

       01  WRK-ERRO-AREA               PIC  X(107)         VALUE SPACES.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           '*** AREA PARA TESTE DE FILE STATUS ***'.
      *----------------------------------------------------------------*

       01  WRK-FS-GFCTSEQE             PIC  X(002)         VALUE SPACES.
       01  WRK-FS-GFCTMVTA             PIC  X(002)         VALUE SPACES.
FAIRES 01  WRK-FS-GFCTMVTE             PIC  X(002)         VALUE SPACES.
       01  WRK-FS-GFCTMVOK             PIC  X(002)         VALUE SPACES.
       01  WRK-FS-GFCTMVRJ             PIC  X(002)         VALUE SPACES.
       01  WRK-FS-GFCTMVER             PIC  X(002)         VALUE SPACES.
       01  WRK-FS-GFCTSEQS             PIC  X(002)         VALUE SPACES.

       01  WRK-ABERTURA                PIC  X(013)         VALUE
           ' NA ABERTURA '.
       01  WRK-LEITURA                 PIC  X(013)         VALUE
           ' NA LEITURA  '.
       01  WRK-GRAVACAO                PIC  X(013)         VALUE
           ' NA GRAVACAO '.
       01  WRK-FECHAMENTO              PIC  X(013)         VALUE
           'NO FECHAMENTO'.

           EJECT
      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           '*** MENSAGEM DE ERRO ***'.
      *----------------------------------------------------------------*

       01  WRK-ERRO-POOL7100.
           03  FILLER                  PIC  X(008)         VALUE
               '** ERRO '.
           03  WRK-OPERACAO            PIC  X(013)         VALUE SPACES.
           03  FILLER                  PIC  X(012)         VALUE
               ' DO ARQUIVO '.
           03  WRK-NOME-ARQ            PIC  X(008)         VALUE SPACES.
           03  FILLER                  PIC  X(018)         VALUE
               ' - FILE STATUS = '.
           03  WRK-FILE-STATUS         PIC  X(002)         VALUE SPACES.
           03  FILLER                  PIC  X(003)         VALUE ' **'.

       01  WRK-MSG-VAZIO               PIC  X(079)         VALUE
           '** ARQUIVO GFCTMVTA VAZIO **'.

       01  WRK-MSG-VAZIO-SEQ           PIC  X(079)         VALUE
           '** ARQUIVO GFCTSEQE VAZIO **'.

       01  WRK-MSG-DIF                 PIC  X(079)         VALUE
           '** ARQUIVO GFCTMVTA - PRIMEIRO REG DIFERENTE DE HEADER **'.

       01  WRK-MSG-TAB                 PIC  X(079)         VALUE
           '** INDEXADOR ULTRAPASSOU 500 OCORRENCIAS **'.

       01  WRK-MSG-9100.
           03  FILLER                  PIC  X(051)         VALUE
           '** ERRO NO ACESSO AO MODULO SCCE9100 **'.
           03  WRK-COD-RET-9100        PIC  9(005)         VALUE ZEROS.
           03  FILLER                  PIC  X(004)         VALUE ' ***'.

       01  WRK-MSG-5005.
           03  FILLER                  PIC  X(054)         VALUE
           '** ERRO NO ACESSO AO MODULO GFCT5005 **'.
           03  WRK-COD-RET-5005        PIC  9(002)         VALUE ZEROS.
           03  FILLER                  PIC  X(004)         VALUE ' ***'.

       01  WRK-MSG-8000.
           03  FILLER                  PIC  X(054)         VALUE
           '** ERRO NO ACESSO AO MODULO SARH8000 **'.
           03  WRK-COD-RET-8000        PIC  9(002)         VALUE ZEROS.
           03  FILLER                  PIC  X(004)         VALUE ' ***'.

       01  WRK-MSG-0536                PIC  X(079)         VALUE
           '** ERRO NO ACESSO AO MODULO POOL0536 **'.

       01  WRK-MSG-9018                PIC  X(079)         VALUE
           '** ERRO NO ACESSO AO MODULO MESU9018 **'.

       01  WRK-MSG-9015.
           03  FILLER                  PIC  X(054)         VALUE
           '** ERRO NO ACESSO AO MODULO MESU9015 ** - COD. RET. = '.
           03  WRK-COD-RET-9015        PIC  9(002)         VALUE ZEROS.
           03  FILLER                  PIC  X(004)         VALUE ' ***'.

       01  WRK-MSG-NTAX.
           03  FILLER                  PIC  X(054)         VALUE
           '** ERRO NO ACESSO AO MODULO NTAX5000 ** - COD. RET. = '.
           03  WRK-COD-RET-NTAX        PIC  9(002)         VALUE ZEROS.
           03  FILLER                  PIC  X(004)         VALUE ' ***'.

       01  WRK-ERRO-GFCT5318.
           03  FILLER                  PIC  X(052)         VALUE
               '*** ERRO NO ACESSO AO MODULO GFCT5318 - COD. RET. = '.
           03  WRK-COD-RET-X           PIC  9(002)         VALUE ZEROS.
           03  FILLER                  PIC  X(004)         VALUE ' ***'.

      *---------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           '*** AREA DA POOL7600 ***'.
      *---------------------------------------------------------------*

       01  WRK-DATA-HORA.
           03  WRK-DT-JULIANA          PIC  9(005) COMP-3  VALUE ZEROS.
           03  WRK-DT-AAMMDD           PIC  9(007) COMP-3  VALUE ZEROS.
           03  WRK-DT-AAAAMMDD         PIC  9(009) COMP-3  VALUE ZEROS.
           03  WRK-TI-HHMMSS           PIC  9(007) COMP-3  VALUE ZEROS.
           03  WRK-TI-HHMMSSMMMMMM     PIC  9(013) COMP-3  VALUE ZEROS.
           03  WRK-TIMESTAMP           PIC  X(020)         VALUE SPACES.


      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           '*** AREA MODULO GFCT5005 ***'.
      *----------------------------------------------------------------*

       01  WRK-5005-AREA-ENTRADA.
           03  WRK-5005-AMBIENTE               PIC  X(001) VALUE SPACES.
           03  WRK-5005-CODIGO-TARIFA          PIC  9(005) VALUE ZEROS.
           03  WRK-5005-DATA-INI-VIGEN         PIC  X(010) VALUE SPACES.

       01  WRK-5005-AREA-SAIDA.
           03  WRK-5005-COD-RETORNO            PIC  9(002) VALUE ZEROS.
           03  WRK-5005-COD-SQL-ERRO           PIC  9(003) VALUE ZEROS.
           03  WRK-5005-DESC-MSG               PIC  X(070) VALUE SPACES.
           03  WRK-5005-DADOS-RETORNO.
               05  WRK-5005-CSERVC-TARIF       PIC  9(005) VALUE ZEROS.
               05  WRK-5005-DINIC-VGCIA-TARIF  PIC  X(010) VALUE SPACES.
               05  WRK-5005-DFIM-VGCIA-TARIF   PIC  X(010) VALUE SPACES.
               05  WRK-5005-CNATUZ-SERVC       PIC  9(003) VALUE ZEROS.
               05  WRK-5005-CCJTO-SERVC        PIC  9(003) VALUE ZEROS.
               05  WRK-5005-CINDCD-DEB-CONSL   PIC  X(001) VALUE SPACES.
               05  WRK-5005-CINDCD-EXCED-CONSL PIC  X(001) VALUE SPACES.
               05  WRK-5005-QFRANQ-TARIF       PIC  9(003) VALUE ZEROS.
               05  WRK-5005-QMAX-TENTV-DEB     PIC  9(003) VALUE ZEROS.
               05  WRK-5005-CPER-FRANQ-TARIF   PIC  9(001) VALUE ZEROS.
               05  WRK-5005-CPER-CALC-TARIF    PIC  9(001) VALUE ZEROS.
               05  WRK-5005-CINIC-PER-CALC     PIC  9(001) VALUE ZEROS.
               05  WRK-5005-DDIA-COBR-TARIF    PIC  9(002) VALUE ZEROS.
JC0413         05  WRK-5005-QDIA-CAREN-TARIF   PIC  9(003) VALUE ZEROS.
               05  WRK-5005-PDESC-CAREN-TARIF  PIC  9(003) VALUE ZEROS.
               05  WRK-5005-CINDCD-INIC-CAREN  PIC  9(001) VALUE ZEROS.
               05  WRK-5005-QDIA-RTCAO-COBR    PIC  9(002) VALUE ZEROS.
               05  WRK-5005-DINIC-CAREN-TARIF  PIC  X(010) VALUE SPACES.
               05  WRK-5005-RSEGDA-LIN-EXTRT   PIC  X(032) VALUE SPACES.
               05  WRK-5005-HIDTFD-MANUT-SERVC PIC  X(026) VALUE SPACES.
               05  WRK-5005-CSEQ-MANUT-SERVC   PIC  9(002) VALUE ZEROS.
               05  WRK-5005-CTPO-FRANQ-TARIF   PIC  9(001) VALUE ZEROS.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           '*** AREA MODULO SCCE9100 ***'.
      *----------------------------------------------------------------*

       COPY 'I#SCCE91'.

           EJECT
      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           '*** AREA MODULO MESU9015 ***'.
      *----------------------------------------------------------------*

       COPY 'I#MESUM6'.

           EJECT
      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           '*** AREA MODULO MESU9018 ****'.
      *----------------------------------------------------------------*

       COPY 'I#MESUM8'.

           EJECT
      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           '*** AREA MODULO NTAX5000 ***'.
      *----------------------------------------------------------------*

       COPY 'I#NTAX00'.

           EJECT
      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           '*** AREA POOL0536 ***'.
      *----------------------------------------------------------------*

       COPY 'I#POOLB4'.

           EJECT
      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           '*** AREA PARA O MODULO GFCT5318 ***'.
      *----------------------------------------------------------------*

       01  WRK-5318-AREA-ENTRADA.
           03  WRK-5318-AMBIENTE       PIC  X(001)     VALUE SPACES.
           03  WRK-5318-CODIGO-TARIFA  PIC  9(005)     VALUE ZEROS.
           03  WRK-5318-DT-REFER       PIC  X(010)     VALUE SPACES.

       01  WRK-5318-AREA-SAIDA.
           03  WRK-5318-COD-RETORNO    PIC  9(002)     VALUE ZEROS.
           03  WRK-5318-COD-SQL-ERRO   PIC  9(003)     VALUE ZEROS.
           03  WRK-5318-DESC-MSG       PIC  X(070)     VALUE SPACES.

       01  WRK-5318-DADOS-RETORNO.
           03  WRK-5318-DADOS-PRINCIPAIS.
               05  WRK-5318-CSERVC-TARIF       PIC  9(005) VALUE ZEROS.
               05  WRK-5318-CBCO-CCREN         PIC  9(003) VALUE ZEROS.
               05  WRK-5318-CIDTFD-LCTO        PIC  9(005) VALUE ZEROS.
               05  WRK-5318-CIDTFD-SNAL-LCTO   PIC  9(001) VALUE ZEROS.
               05  WRK-5318-CEMPR-INC          PIC  9(005) VALUE ZEROS.
               05  WRK-5318-CDEPDC             PIC  9(005) VALUE ZEROS.
               05  WRK-5318-CTPO-SERVC-TARIF   PIC  9(001) VALUE ZEROS.
               05  WRK-5318-RSERVC-TARIF       PIC  X(255) VALUE SPACES.
               05  WRK-5318-RSERVC-TARIF-REDZD PIC  X(100) VALUE SPACES.
               05  WRK-5318-CPSSOA-SERVC-TARIF PIC  X(001) VALUE SPACES.
               05  WRK-5318-CTPO-CTA-DEB-TARIF PIC  9(001) VALUE ZEROS.
               05  WRK-5318-CTPO-DEB-COBR      PIC  9(001) VALUE ZEROS.
               05  WRK-5318-RUND-COBR-SERVC    PIC  X(020) VALUE SPACES.
               05  WRK-5318-CLCTO-SERVC        PIC  9(005) VALUE ZEROS.
               05  WRK-5318-HIDTFD-MANUT-SERVC-R
                                               PIC  X(026) VALUE SPACES.
               05  WRK-5318-CSEQ-MANUT-SERVC-R PIC  9(002) VALUE ZEROS.
               05  WRK-5318-CTPO-LCTO-COBR     PIC  9(002) VALUE ZEROS.
               05  WRK-5318-CTPO-LCTO-ESTRN    PIC  9(002) VALUE ZEROS.
               05  WRK-5318-CCART-DEB-TARIF    PIC  9(005) VALUE ZEROS.
               05  WRK-5318-CINDCD-DEB-PCIAL   PIC  X(001) VALUE SPACES.
               05  WRK-5318-CINDCD-PCIAL-ULT   PIC  X(001) VALUE SPACES.
               05  WRK-5318-CINDCD-EVNTO-PCELD PIC  X(001) VALUE SPACES.
               05  WRK-5318-CINDCD-QTD-REG     PIC  X(001) VALUE SPACES.
               05  WRK-5318-CINDCD-NRO-DOCTO   PIC  X(001) VALUE SPACES.

           03  WRK-5318-DADOS-DE-PACOTE  OCCURS   50.
               05  WRK-5318-DINIC-VGCIA-COMPS  PIC  X(010) VALUE SPACES.
               05  WRK-5318-CSEQ-COMPO-PCOTE   PIC  9(003) VALUE ZEROS.
               05  WRK-5318-DFIM-VGCIA-COMPS   PIC  X(010) VALUE SPACES.
               05  WRK-5318-CGRP-SERVC         PIC  9(005) VALUE ZEROS.
               05  WRK-5318-DINIC-VGCIA-SERVC  PIC  X(010) VALUE SPACES.
               05  WRK-5318-CTARIF-COMPO-PCOTE PIC  9(005) VALUE ZEROS.
               05  WRK-5318-QFRANQ-COMPO-PCOTE PIC  9(003) VALUE ZEROS.
               05  WRK-5318-CPER-COBR-COMPO    PIC  9(001) VALUE ZEROS.
               05  WRK-5318-VEXCED-COMPO-PCOTE PIC  9(011)V9(002)
                                                           VALUE ZEROS.
               05  WRK-5318-PEXCED-COMPO-PCOTE PIC  9(007)V9(004)
                                                           VALUE ZEROS.
               05  WRK-5318-QDIA-COBR-COMPO    PIC  9(002) VALUE ZEROS.
               05  WRK-5318-RSEGDA-LIN-EXCED   PIC  X(032) VALUE SPACES.
               05  WRK-5318-HIDTFD-MANUT-SERVC PIC  X(026) VALUE SPACES.
               05  WRK-5318-CSEQ-MANUT-SERVC   PIC  9(002) VALUE ZEROS.

           03  WRK-5318-DADOS-DE-PARAMENTROS.
               05  WRK-5318-DINIC-VGCIA-TARIF-P
                                               PIC  X(010) VALUE SPACES.
               05  WRK-5318-DFIM-VGCIA-TARIF   PIC  X(010) VALUE SPACES.
               05  WRK-5318-CNATUZ-SERVC       PIC  9(003) VALUE ZEROS.
               05  WRK-5318-CCJTO-SERVC        PIC  9(003) VALUE ZEROS.
               05  WRK-5318-CINDCD-DEB-CONSL   PIC  X(001) VALUE SPACES.
               05  WRK-5318-CINDCD-EXCED-CONSL PIC  X(001) VALUE SPACES.
               05  WRK-5318-QFRANQ-TARIF       PIC  9(003) VALUE ZEROS.
               05  WRK-5318-QMAX-TENTV-DEB     PIC  9(003) VALUE ZEROS.
               05  WRK-5318-CPER-FRANQ-TARIF   PIC  9(001) VALUE ZEROS.
               05  WRK-5318-CPER-CALC-TARIF    PIC  9(001) VALUE ZEROS.
               05  WRK-5318-CINIC-PER-CALC     PIC  9(001) VALUE ZEROS.
               05  WRK-5318-DDIA-COBR-TARIF    PIC  9(002) VALUE ZEROS.
JC0413         05  WRK-5318-QDIA-CAREN-TARIF   PIC  9(003) VALUE ZEROS.
               05  WRK-5318-PDESC-CAREN-TARIF  PIC  9(007)V9(004)
                                                           VALUE ZEROS.
               05  WRK-5318-CINDCD-INIC-CAREN  PIC  9(001) VALUE ZEROS.
               05  WRK-5318-QDIA-RTCAO-COBR    PIC  9(002) VALUE ZEROS.
               05  WRK-5318-DINIC-CAREN-TARIF  PIC  X(010) VALUE SPACES.
               05  WRK-5318-RSEGDA-LIN-EXTRT   PIC  X(032) VALUE SPACES.
               05  WRK-5318-HIDTFD-MANUT-SERVC-P
                                               PIC  X(026) VALUE SPACES.
               05  WRK-5318-CSEQ-MANUT-SERVC-P PIC  9(002) VALUE ZEROS.
               05  WRK-5318-CINIC-PER-FRANQ    PIC  9(001) VALUE ZEROS.
               05  WRK-5318-VMIN-ENVIO-COBR    PIC  9(009)V9(2)
                                                           VALUE ZEROS.
               05  WRK-5318-CINDCD-TARIF-PROML PIC  X(001) VALUE SPACE.
               05  WRK-5318-CINDCD-TENTV-COBR  PIC  X(001) VALUE SPACE.
               05  WRK-5318-QTENTV-SEM-COBR    PIC  9(003) VALUE ZEROS.
               05  WRK-5318-CINDCD-RETOR-COBR  PIC  X(001) VALUE SPACE.
               05  WRK-5318-CINDCD-CALC-PRORT  PIC  X(001) VALUE SPACE.
               05  WRK-5318-QDIA-COBR-TARIF    PIC  9(002) VALUE ZEROS.
               05  WRK-5318-CIDTFD-COBR-FERI   PIC  X(001) VALUE SPACE.
               05  WRK-5318-CTPO-FRANQ-TARIF   PIC  9(001) VALUE ZEROS.

           03  WRK-5318-DADOS-DE-VALORES       OCCURS   50.
               05  WRK-5318-DINIC-VGCIA-TARIF  PIC  X(010) VALUE SPACES.
               05  WRK-5318-CSEQ-COMPO-TARIF   PIC  9(002) VALUE ZEROS.
               05  WRK-5318-CMOEDA-BACEN       PIC  9(005) VALUE ZEROS.
               05  WRK-5318-RCOMPO-TARIF       PIC  X(255) VALUE SPACES.
               05  WRK-5318-VCOMPO-TARIF       PIC  9(011)V9(002)
                                                           VALUE ZEROS.
               05  WRK-5318-PSERVC-PTADO       PIC  9(007)V9(004)
                                                           VALUE ZEROS.
               05  WRK-5318-VMAX-COMPO-TARIF   PIC  9(011)V9(002)
                                                           VALUE ZEROS.
               05  WRK-5318-VMIN-COMPO-TARIF   PIC  9(011)V9(002)
                                                           VALUE ZEROS.
               05  WRK-5318-CMOMEN-CONVS-MOEDA PIC  9(001) VALUE ZEROS.
      *
           EJECT
AT0522*----------------------------------------------------------------*
AT0522 01  FILLER                      PIC X(050)  VALUE
AT0522     'AREA DE INTERFACE COM SERVICO FUNCIONAL CARO8200'.
AT0522*----------------------------------------------------------------*
AT0522
AT0522 01  WRK-CARO8200                PIC  X(008)         VALUE
AT0522     'CARO8200'.
AT0522
AT0522     COPY 'CAROW820'.
      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           '*** AREA PARA TRATAMENTO DE ERRO ***'.
      *----------------------------------------------------------------*

       COPY 'POL7100C'.

           EJECT
      *---------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           '*** AREA DE DB2 ***'.
      *---------------------------------------------------------------*

           EXEC SQL
               INCLUDE SQLCA
           END-EXEC.
           EJECT

           EXEC SQL
               INCLUDE GFCTB094
           END-EXEC.
           EJECT

           EXEC SQL
               INCLUDE GFCTB0A2
           END-EXEC.
           EJECT

           EXEC SQL
               INCLUDE CLIEV007
           END-EXEC.
           EJECT

           EXEC SQL
               INCLUDE CLIEV008
           END-EXEC.
           EJECT

           EXEC SQL
               INCLUDE CLIEV004
           END-EXEC.
           EJECT

           EXEC SQL
               INCLUDE GFCTB0F4
           END-EXEC.
           EJECT

           EXEC SQL
               INCLUDE GFCTB0A1
           END-EXEC.
           EJECT

           EXEC SQL
               INCLUDE GFCTB0G8
           END-EXEC.
           EJECT

           EXEC SQL
               INCLUDE GFCTB0G9
           END-EXEC.
           EJECT

           EXEC SQL
               INCLUDE SBINB026
           END-EXEC.
           EJECT

           EXEC SQL
               INCLUDE GFCTB0K7
           END-EXEC.
           EJECT

           EXEC SQL
               INCLUDE GFCTB0F0
           END-EXEC.

           EXEC SQL
               INCLUDE GFCTB0D8
           END-EXEC.

FAIRES*----------------------------------------------------------------*
  "    LINKAGE                   SECTION.
  "   *----------------------------------------------------------------*
  "    01  LNK-PARM.
  "        05   LNK-TAMANHO            PIC S9(004) COMP.
  "        05   LNK-MODULO             PIC  X(004).
FAIRES


      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           '*** GFCT1105 - FIM DA AREA DE WORKING ***'.
      *----------------------------------------------------------------*
           EJECT
      *================================================================*
FAIRES PROCEDURE                       DIVISION  USING LNK-PARM.
      *================================================================*
           MOVE '+' TO
               WRK-CAMPO-TIRA-SINAL-S
               WRK-CAMPO-POE-SINAL-01-S
               WRK-CAMPO-POE-SINAL-02-S
               WRK-CAMPO-POE-SINAL-03-S
               WRK-CAMPO-POE-SINAL-04-S
               WRK-CAMPO-POE-SINAL-05-S
               WRK-CAMPO-POE-SINAL-06-S
               WRK-CAMPO-POE-SINAL-07-S
               WRK-CAMPO-POE-SINAL-08-S
               WRK-CAMPO-POE-SINAL-09-S
               WRK-CAMPO-POE-SINAL-10-S
               WRK-CAMPO-POE-SINAL-11-S
               WRK-CAMPO-POE-SINAL-12-S
               WRK-CAMPO-POE-SINAL-13-S
               WRK-CAMPO-POE-SINAL-14-S
               WRK-CAMPO-POE-SINAL-15-S
               WRK-CAMPO-POE-SINAL-16-S
               WRK-CAMPO-POE-SINAL-17-S.

           MOVE ZEROS TO
               WRK-CAMPO-TIRA-SINAL
               WRK-CAMPO-TIRA-SINAL-17
               WRK-CAMPO-TIRA-SINAL-16
               WRK-CAMPO-TIRA-SINAL-15
               WRK-CAMPO-TIRA-SINAL-14
               WRK-CAMPO-TIRA-SINAL-13
               WRK-CAMPO-TIRA-SINAL-12
               WRK-CAMPO-TIRA-SINAL-11
               WRK-CAMPO-TIRA-SINAL-10
               WRK-CAMPO-TIRA-SINAL-09
               WRK-CAMPO-TIRA-SINAL-08
               WRK-CAMPO-TIRA-SINAL-07
               WRK-CAMPO-TIRA-SINAL-06
               WRK-CAMPO-TIRA-SINAL-05
               WRK-CAMPO-TIRA-SINAL-04
               WRK-CAMPO-TIRA-SINAL-03
               WRK-CAMPO-TIRA-SINAL-02
               WRK-CAMPO-TIRA-SINAL-01
               WRK-CAMPO-POE-SINAL-01
               WRK-CAMPO-POE-SINAL-01-C
               WRK-CAMPO-POE-SINAL-02
               WRK-CAMPO-POE-SINAL-02-C
               WRK-CAMPO-POE-SINAL-03
               WRK-CAMPO-POE-SINAL-03-C
               WRK-CAMPO-POE-SINAL-04
               WRK-CAMPO-POE-SINAL-04-C
               WRK-CAMPO-POE-SINAL-05
               WRK-CAMPO-POE-SINAL-05-C
               WRK-CAMPO-POE-SINAL-06
               WRK-CAMPO-POE-SINAL-06-C
               WRK-CAMPO-POE-SINAL-07
               WRK-CAMPO-POE-SINAL-07-C
               WRK-CAMPO-POE-SINAL-08
               WRK-CAMPO-POE-SINAL-08-C
               WRK-CAMPO-POE-SINAL-09
               WRK-CAMPO-POE-SINAL-09-C
               WRK-CAMPO-POE-SINAL-10
               WRK-CAMPO-POE-SINAL-10-C
               WRK-CAMPO-POE-SINAL-11
               WRK-CAMPO-POE-SINAL-11-C
               WRK-CAMPO-POE-SINAL-12
               WRK-CAMPO-POE-SINAL-12-C
               WRK-CAMPO-POE-SINAL-13
               WRK-CAMPO-POE-SINAL-13-C
               WRK-CAMPO-POE-SINAL-14
               WRK-CAMPO-POE-SINAL-14-C
               WRK-CAMPO-POE-SINAL-15
               WRK-CAMPO-POE-SINAL-15-C
               WRK-CAMPO-POE-SINAL-16
               WRK-CAMPO-POE-SINAL-16-C
               WRK-CAMPO-POE-SINAL-17
               WRK-CAMPO-POE-SINAL-17-C.

      *----------------------------------------------------------------*
       000000-ROTINA-PRINCIPAL         SECTION.
      *----------------------------------------------------------------*

           PERFORM 100000-INICIALIZAR.

           PERFORM 200000-VERIFICAR-VAZIO.

           PERFORM  352990-ACESSA-DPROCM.

           PERFORM 300000-PROCESSAR    UNTIL
                   WRK-FS-GFCTMVTA     EQUAL '10' OR
FAIRES             WRK-FS-GFCTMVTE     EQUAL '10'.

           PERFORM 400000-FINALIZAR.

      *----------------------------------------------------------------*
       000000-99-FIM.                  EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       4200-PESQUISA-CORRELATA SECTION.
      *----------------------------------------------------------------*

           MOVE 'N' TO WRK-ACHOU
           MOVE GFCT00-EVN-COD-TARIFA-5 OF GFCTMVTA TO
                WRK-S5-5-R
           MOVE '+'      TO WRK-S5-5-SINAL
           MOVE WRK-S5-5 TO WRK-S5-5-S
           PERFORM VARYING WRK-CTN FROM 1 BY 1 UNTIL
                           WRK-ACHOU = 'S' OR
                           WRK-CTN GREATER WRK-CTM
              IF WRK-S5-5-S                          EQUAL
                 BD1-CSERVC-TARIF-CORRL(WRK-CTN)     AND
                 WRK-CCUSTO                          EQUAL
                 BD1-CSIST-CORRL(WRK-CTN)
                 MOVE 'S' TO WRK-ACHOU
                 IF BD1-CSERVC-TARIF(WRK-CTN) NOT LESS 1000 AND
                    BD1-CTPO-SERVC-TARIF(WRK-CTN) EQUAL 1
                    MOVE BD1-CSERVC-TARIF(WRK-CTN) TO
                         WRK-S5-5
                    MOVE WRK-S5-5-R                TO
                         GFCT00-EVN-CD-TARI-CORR-R OF GFCTMVTA
                    ADD 1 TO BD1-QTDE-CONV(WRK-CTN)
                 END-IF
              END-IF
           END-PERFORM.

           IF WRK-ACHOU EQUAL 'N'
              ADD 1 TO WRK-CTM
              IF WRK-CTM GREATER 1000
                 MOVE 8                  TO RETURN-CODE
                                            WRK-COD-RET-9100
                 MOVE 'ESTOURO INDICE TAR.CORRELATA' TO ERR-TEXTO
                 PERFORM 999999-PROCESSAR-ROTINA-ERRO
              END-IF
              MOVE WRK-CCUSTO TO CSIST-CORRL OF GFCTB0F0
                                 BD1-CSIST-CORRL(WRK-CTM)
              MOVE GFCT00-EVN-COD-TARIFA-5 OF GFCTMVTA TO
                   CSERVC-TARIF-CORRL OF GFCTB0F0
                   WRK-S5-5-R
              MOVE WRK-S5-5 TO
                   BD1-CSERVC-TARIF-CORRL(WRK-CTM)
              MOVE ZEROS      TO BD1-CSERVC-TARIF(WRK-CTM)
                                 BD1-CTPO-SERVC-TARIF(WRK-CTM)
                                 BD1-QTDE-CONV(WRK-CTM)
              PERFORM 4300-PESQUISA-GFCTB0F0
              IF WRK-ACHOU = 'S'
                 MOVE CSERVC-TARIF OF GFCTB0F0 TO
                      BD1-CSERVC-TARIF(WRK-CTM)
                      CSERVC-TARIF OF GFCTB0D8
                 IF BD1-CSERVC-TARIF(WRK-CTM) NOT LESS 1000
                    PERFORM 4400-PESQUISA-GFCTB0D8
                    IF BD1-CTPO-SERVC-TARIF(WRK-CTM) EQUAL 1
                        MOVE BD1-CSERVC-TARIF(WRK-CTM) TO
                            WRK-S5-5
                        MOVE WRK-S5-5-R TO
                            GFCT00-EVN-CD-TARI-CORR-R OF GFCTMVTA
                        ADD 1 TO BD1-QTDE-CONV(WRK-CTM)
                    END-IF
                 END-IF
              END-IF
           END-IF.
      *----------------------------------------------------------------*
       4200-99-FIM.                  EXIT.
      *----------------------------------------------------------------*
           EJECT

      *----------------------------------------------------------------*
       4300-PESQUISA-GFCTB0F0    SECTION.
      *----------------------------------------------------------------*
           EXEC SQL
             SELECT
                   CSERVC_TARIF
             INTO
                   :GFCTB0F0.CSERVC-TARIF
             FROM   DB2PRD.TARIF_CORRL_SIST
             WHERE
                   CSIST_CORRL    = :GFCTB0F0.CSIST-CORRL AND
                   CSERVC_TARIF_CORRL = :GFCTB0F0.CSERVC-TARIF-CORRL
           END-EXEC.

           IF (SQLCODE NOT EQUAL ZEROS AND +100) OR
              (SQLWARN0                EQUAL 'W')
                MOVE 8                  TO RETURN-CODE
                MOVE 'DB2'              TO ERR-TIPO-ACESSO
                MOVE 'TARIF_CORRL_SIST' TO ERR-DBD-TAB
                MOVE 'SELECT  '         TO ERR-FUN-COMANDO
                MOVE  SQLCODE           TO ERR-SQL-CODE
                MOVE  '1010'            TO ERR-LOCAL
                MOVE  SPACES            TO ERR-SEGM
                PERFORM 999999-PROCESSAR-ROTINA-ERRO
           END-IF.

           IF  SQLCODE                 EQUAL ZEROS
               MOVE 'S'  TO  WRK-ACHOU
           ELSE
               MOVE 'N'  TO  WRK-ACHOU
           END-IF.

      *----------------------------------------------------------------*
       4300-99-FIM.                  EXIT.
      *----------------------------------------------------------------*
           EJECT

      *----------------------------------------------------------------*
       4400-PESQUISA-GFCTB0D8    SECTION.
      *----------------------------------------------------------------*
           EXEC SQL
             SELECT
                   CTPO_SERVC_TARIF
             INTO  :GFCTB0D8.CTPO-SERVC-TARIF
             FROM  DB2PRD.SERVC_TARIF_PRINC
             WHERE CSERVC_TARIF         = :GFCTB0D8.CSERVC-TARIF
           END-EXEC.

           IF (SQLCODE NOT EQUAL ZEROS AND +100) OR
              (SQLWARN0                EQUAL 'W')
                MOVE 8                  TO RETURN-CODE
                MOVE 'DB2'              TO ERR-TIPO-ACESSO
                MOVE 'SERVC_TARIF_PRINC' TO ERR-DBD-TAB
                MOVE 'SELECT  '         TO ERR-FUN-COMANDO
                MOVE  SQLCODE           TO ERR-SQL-CODE
                MOVE  '1020'            TO ERR-LOCAL
                MOVE  SPACES            TO ERR-SEGM
                PERFORM 999999-PROCESSAR-ROTINA-ERRO
           END-IF.

           IF  SQLCODE                 EQUAL ZEROS
               MOVE CTPO-SERVC-TARIF OF GFCTB0D8 TO
                    BD1-CTPO-SERVC-TARIF(WRK-CTM)
           END-IF.


      *----------------------------------------------------------------*
       4400-99-FIM.                  EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       4500-DISPLAY-CORRELATAS SECTION.
      *----------------------------------------------------------------*

           DISPLAY ' '.
           DISPLAY WRK-DISP-CORR-AST-1.
           DISPLAY WRK-DISP-CORR-AST-2.
           DISPLAY WRK-DISP-CORR-AST-3.
           DISPLAY WRK-DISP-CORR-AST-4.

           PERFORM VARYING WRK-CTN FROM 1 BY 1 UNTIL
                           WRK-CTN GREATER WRK-CTM
               MOVE WRK-CTN TO WRK-D-CORR-IDX
               MOVE BD1-CSIST-CORRL(WRK-CTN) TO
                    WRK-D-CSIST-CORRL
               MOVE BD1-CSERVC-TARIF-CORRL(WRK-CTN) TO
                    WRK-D-CERVC-TARIF-CORRL
               MOVE BD1-CSERVC-TARIF(WRK-CTN) TO
                    WRK-D-CSERVC-TARIF
               MOVE BD1-CTPO-SERVC-TARIF(WRK-CTN) TO
                    WRK-D-CTPO-SERVC-TARIF
               MOVE BD1-QTDE-CONV(WRK-CTN) TO
                    WRK-D-QTDE-CONV
               DISPLAY WRK-DISP-CORR-AST-5
           END-PERFORM.

           DISPLAY WRK-DISP-CORR-AST-1.
           DISPLAY WRK-DISP-CORR-AST-2.
           DISPLAY WRK-DISP-CORR-AST-3.
           DISPLAY ' '.

      *----------------------------------------------------------------*
       4500-99-FIM.                  EXIT.
      *----------------------------------------------------------------*


      *----------------------------------------------------------------*
       100000-INICIALIZAR              SECTION.
      *----------------------------------------------------------------*

           CALL 'POOL1050'.

FAIRES     IF   LNK-MODULO EQUAL 'PERF'
  "             OPEN INPUT  GFCTMVTA
  "                         GFCTMVTE
  "                         GFCTSEQE
  "                  OUTPUT GFCTMVOK
  "                         GFCTSEQS
  "                         GFCTMVRJ
  "                         GFCTMVER
  "        ELSE
  "             OPEN INPUT  GFCTMVTA
  "                         GFCTSEQE
  "                  OUTPUT GFCTMVOK
  "                         GFCTSEQS
  "                         GFCTMVRJ
  "                         GFCTMVER
FAIRES     END-IF.
           MOVE WRK-ABERTURA           TO WRK-OPERACAO.

           INITIALIZE WRK-TAB-GRUPO

           PERFORM 110000-TESTAR-FILE-STATUS.

           PERFORM 150000-OBTER-DATA.

      *----------------------------------------------------------------*
       100000-99-FIM.                  EXIT.
      *----------------------------------------------------------------*
           EJECT
      *----------------------------------------------------------------*
       110000-TESTAR-FILE-STATUS       SECTION.
      *----------------------------------------------------------------*

FAIRES     IF  LNK-MODULO  EQUAL 'PERF'
  "            PERFORM 111200-TESTAR-FS-GFCTMVTE
  "        ELSE
  "            PERFORM 111000-TESTAR-FS-GFCTMVTA
FAIRES     END-IF.

           PERFORM 111500-TESTAR-FS-GFCTSEQE.

           PERFORM 112000-TESTAR-FS-GFCTMVOK.

           PERFORM 113000-TESTAR-FS-GFCTMVRJ.

           PERFORM 114000-TESTAR-FS-GFCTMVER.

           PERFORM 115000-TESTAR-FS-GFCTSEQS.

      *----------------------------------------------------------------*
       110000-99-FIM.                  EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       111000-TESTAR-FS-GFCTMVTA       SECTION.
      *----------------------------------------------------------------*

           IF  WRK-FS-GFCTMVTA         NOT EQUAL   '00'
               MOVE 'GFCTMVTA'         TO WRK-NOME-ARQ
               MOVE WRK-FS-GFCTMVTA    TO WRK-FILE-STATUS
               MOVE WRK-ERRO-POOL7100  TO ERR-TEXTO
               PERFORM 999999-PROCESSAR-ROTINA-ERRO
           END-IF.


FAIRES*----------------------------------------------------------------*
  "    111200-TESTAR-FS-GFCTMVTE       SECTION.
  "   *----------------------------------------------------------------*
  "
  "        IF  WRK-FS-GFCTMVTE         NOT EQUAL   '00'
  "            MOVE 'GFCTMVTE'         TO WRK-NOME-ARQ
  "            MOVE WRK-FS-GFCTMVTE    TO WRK-FILE-STATUS
  "            MOVE WRK-ERRO-POOL7100  TO ERR-TEXTO
  "            PERFORM 999999-PROCESSAR-ROTINA-ERRO
  "        END-IF.
FAIRES

      *----------------------------------------------------------------*
       111500-TESTAR-FS-GFCTSEQE       SECTION.
      *----------------------------------------------------------------*

           IF  WRK-FS-GFCTSEQE         NOT EQUAL '00'
               MOVE 'GFCTSEQE'         TO WRK-NOME-ARQ
               MOVE WRK-FS-GFCTSEQE    TO WRK-FILE-STATUS
               MOVE WRK-ERRO-POOL7100  TO ERR-TEXTO
               PERFORM 999999-PROCESSAR-ROTINA-ERRO
           END-IF.

      *----------------------------------------------------------------*
       111500-99-FIM.                  EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       112000-TESTAR-FS-GFCTMVOK       SECTION.
      *----------------------------------------------------------------*

           IF  WRK-FS-GFCTMVOK         NOT EQUAL '00'
               MOVE 'GFCTMVOK'         TO WRK-NOME-ARQ
               MOVE WRK-FS-GFCTMVOK    TO WRK-FILE-STATUS
               MOVE WRK-ERRO-POOL7100  TO ERR-TEXTO
               PERFORM 999999-PROCESSAR-ROTINA-ERRO
           END-IF.

      *----------------------------------------------------------------*
       112000-99-FIM.                  EXIT.
      *----------------------------------------------------------------*
           EJECT
      *----------------------------------------------------------------*
       113000-TESTAR-FS-GFCTMVRJ       SECTION.
      *----------------------------------------------------------------*

           IF  WRK-FS-GFCTMVRJ         NOT EQUAL '00'
               MOVE 'GFCTMVRJ'         TO WRK-NOME-ARQ
               MOVE WRK-FS-GFCTMVRJ    TO WRK-FILE-STATUS
               MOVE WRK-ERRO-POOL7100  TO ERR-TEXTO
               PERFORM 999999-PROCESSAR-ROTINA-ERRO
           END-IF.

      *----------------------------------------------------------------*
       113000-99-FIM.                  EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       114000-TESTAR-FS-GFCTMVER       SECTION.
      *----------------------------------------------------------------*

           IF  WRK-FS-GFCTMVER         NOT EQUAL '00'
               MOVE 'GFCTMVER'         TO WRK-NOME-ARQ
               MOVE WRK-FS-GFCTMVER    TO WRK-FILE-STATUS
               MOVE WRK-ERRO-POOL7100  TO ERR-TEXTO
               PERFORM 999999-PROCESSAR-ROTINA-ERRO
           END-IF.

      *----------------------------------------------------------------*
       114000-99-FIM.                  EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       115000-TESTAR-FS-GFCTSEQS       SECTION.
      *----------------------------------------------------------------*

           IF  WRK-FS-GFCTSEQS         NOT EQUAL '00'
               MOVE 'GFCTSEQS'         TO WRK-NOME-ARQ
               MOVE WRK-FS-GFCTSEQS    TO WRK-FILE-STATUS
               MOVE WRK-ERRO-POOL7100  TO ERR-TEXTO
               PERFORM 999999-PROCESSAR-ROTINA-ERRO
           END-IF.

      *----------------------------------------------------------------*
       115000-99-FIM.                  EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       150000-OBTER-DATA               SECTION.
      *----------------------------------------------------------------*

           CALL 'POOL7600'             USING WRK-DATA-HORA.
           MOVE WRK-DT-AAAAMMDD        TO WRK-DATA1-AAAAMMDD.
           MOVE WRK-TI-HHMMSS          TO WRK-HORA1-AUX.
           MOVE WRK-HH1                TO WRK-HH2.
           MOVE WRK-MM1                TO WRK-MM2.
           MOVE WRK-SS1                TO WRK-SS2.

      *----------------------------------------------------------------*
       150000-99-FIM.                  EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       200000-VERIFICAR-VAZIO          SECTION.
      *----------------------------------------------------------------*

           PERFORM 210000-LER-GFCTMVTA.
FAIRES     IF  LNK-MODULO        EQUAL 'PERF'
  "            IF  WRK-FS-GFCTMVTE   EQUAL '10'
  "                DISPLAY '*************** GFCT1105 ***************'
  "                DISPLAY '*                                      *'
  "                DISPLAY '*        ARQUIVO GFCTMVTE VAZIO        *'
  "                DISPLAY '*        PROCESSAMENTO CANCELADO       *'
  "                DISPLAY '*                                      *'
  "                DISPLAY '*************** GFCT1105 ***************'
  "                MOVE 8                  TO RETURN-CODE
  "                MOVE WRK-MSG-VAZIO      TO ERR-TEXTO
  "                PERFORM 999999-PROCESSAR-ROTINA-ERRO
  "             END-IF
  "        ELSE
  "            IF  WRK-FS-GFCTMVTA   EQUAL '10'
  "                DISPLAY '*************** GFCT1105 ***************'
  "                DISPLAY '*                                      *'
  "                DISPLAY '*        ARQUIVO GFCTMVTA VAZIO        *'
  "                DISPLAY '*        PROCESSAMENTO CANCELADO       *'
  "                DISPLAY '*                                      *'
  "                DISPLAY '*************** GFCT1105 ***************'
  "                MOVE 8                  TO RETURN-CODE
  "                MOVE WRK-MSG-VAZIO      TO ERR-TEXTO
  "                PERFORM 999999-PROCESSAR-ROTINA-ERRO
  "            END-IF
FAIRES     END-IF.


           IF  GFCT00-HDR-TP-REG       OF GFCTMVTA
                                       NOT EQUAL ZEROS
               DISPLAY '*********************** GFCT1105 ***************
      -        '*********'
               DISPLAY '*
      -        '        *'
               DISPLAY '*  ARQUIVO GFCTMVTA - PRIMEIRO REG DIFERENTE DE
      -        'HEADER  *'
               DISPLAY '*               PROCESSAMENTO CANCELADO
      -        '        *'
               DISPLAY '*
      -        '        *'
               DISPLAY '*********************** GFCT1105 ***************
      -        '*********'
               MOVE 8                  TO RETURN-CODE
               MOVE WRK-MSG-DIF        TO ERR-TEXTO
               PERFORM 999999-PROCESSAR-ROTINA-ERRO
           END-IF.

           PERFORM 210500-LER-GFCTSEQE.

           IF  WRK-IND-FIM-ARQ-SEQ     EQUAL 'S'
               DISPLAY '*************** GFCT1105 ***************'
               DISPLAY '*                                      *'
               DISPLAY '*        ARQUIVO GFCTSEQE VAZIO        *'
               DISPLAY '*        PROCESSAMENTO CANCELADO       *'
               DISPLAY '*                                      *'
               DISPLAY '*************** GFCT1105 ***************'
               MOVE 8                  TO RETURN-CODE
               MOVE WRK-MSG-VAZIO-SEQ  TO ERR-TEXTO
               PERFORM 999999-PROCESSAR-ROTINA-ERRO
           END-IF.

      *----------------------------------------------------------------*
       200000-99-FIM.                  EXIT.
      *----------------------------------------------------------------*
           EJECT
      *----------------------------------------------------------------*
       210000-LER-GFCTMVTA             SECTION.
      *----------------------------------------------------------------*

FAIRES     IF LNK-MODULO   EQUAL 'PERF'
  "           READ GFCTMVTE       INTO    WRK-REG-GFCTMVTE
  "           IF  WRK-FS-GFCTMVTE         EQUAL '10'
  "               MOVE 'S'                TO WRK-IND-FIM-ARQ-MOV
  "               GO TO 210000-99-FIM
  "           END-IF
  "           MOVE WRK-LEITURA            TO WRK-OPERACAO
  "           PERFORM 111200-TESTAR-FS-GFCTMVTE
  "           MOVE    WRK-PRIPARTE-GFCTMVTE  TO  GFCT00-REG OF GFCTMVTA
  "        ELSE
  "           READ GFCTMVTA
  "           IF  WRK-FS-GFCTMVTA         EQUAL '10'
  "               MOVE 'S'                TO WRK-IND-FIM-ARQ-MOV
  "               GO TO 210000-99-FIM
  "           END-IF
  "           MOVE WRK-LEITURA            TO WRK-OPERACAO
  "           PERFORM 111000-TESTAR-FS-GFCTMVTA
FAIRES     END-IF.

           ADD 1                       TO WRK-CT-LIDOS.
PR2801     ADD 1                       TO ACU-COMMIT.

______*____IF_GFCT00-TRL-TP-REG__OF_GFCTMVTA_EQUAL_1_AND
______*_______GFCT00-EVN-SEQ-REG_OF_GFCTMVTA_EQUAL_13
______*_______NEXT_SENTENCE
______*____END-IF.

           EVALUATE GFCT00-TRL-TP-REG  OF GFCTMVTA
               WHEN ZEROS
                   ADD 1               TO WRK-CT-LIDOS-0
                   MOVE GFCT00-HDR-ROT-ORIG  OF GFCTMVTA TO
                        WRK-CCUSTO-X

               WHEN 1
                   ADD 1               TO WRK-CT-LIDOS-1
                   ADD 1               TO WRK-CT-EVN-RECBD
                   MOVE ZEROS          TO GFCT00-EVN-CD-TARI-CORR-R
                                          OF GFCTMVTA
                   IF WRK-CCUSTO NOT EQUAL 'GFCT' AND
                      GFCT00-EVN-COD-TARIFA-5 OF GFCTMVTA NOT LESS 1000
                      PERFORM 4200-PESQUISA-CORRELATA
                   END-IF
               WHEN 2
                   ADD 1               TO WRK-CT-LIDOS-2

               WHEN 3
                   ADD 1               TO WRK-CT-LIDOS-3

               WHEN 4
                   ADD 1               TO WRK-CT-LIDOS-4

               WHEN 9
                   ADD 1               TO WRK-CT-LIDOS-9
           END-EVALUATE.

      *----------------------------------------------------------------*
       210000-99-FIM.                  EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       210500-LER-GFCTSEQE             SECTION.
      *----------------------------------------------------------------*

           READ GFCTSEQE.

           IF  WRK-FS-GFCTSEQE         EQUAL '10'
               MOVE 'S'                TO WRK-IND-FIM-ARQ-SEQ
               GO TO 210500-99-FIM
           END-IF.

           MOVE WRK-LEITURA            TO WRK-OPERACAO.

           PERFORM 111500-TESTAR-FS-GFCTSEQE.

           ADD 1                       TO WRK-CT-LIDOS-SEQ.

      *----------------------------------------------------------------*
       210500-99-FIM.                  EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       300000-PROCESSAR                SECTION.
      *----------------------------------------------------------------*

           IF  GFCT00-HDR-TP-REG       OF GFCTMVTA
                                       EQUAL 9
               MOVE GFCT00-TRL-IND-ARQ-REJ
                                       OF GFCTMVTA
                                       TO WRK-INDARQ-REJ.

           IF  (GFCT00-HDR-TP-REG      OF GFCTMVTA
                                       EQUAL 1 OR 9) AND
               (IND-1                  GREATER ZEROS)
               MOVE ZEROS              TO IND-1
               PERFORM 310000-DESCARREGAR-TAB-GP
           END-IF.

           IF  GFCT00-HDR-TP-REG       OF GFCTMVTA
                                       EQUAL 0 OR 1
               PERFORM 305000-GUARDA-CAMPOS.

           IF  GFCT00-HDR-TP-REG       OF GFCTMVTA
                                       EQUAL ZEROS OR 9
               PERFORM 320000-GRAVAR-GFCTMVOK-1
               PERFORM 330000-GRAVAR-GFCTMVRJ-1
               IF  GFCT00-HDR-TP-REG   OF GFCTMVTA
                                       EQUAL 9
                   PERFORM 370000-FORMATA-ARQ-CTRL-SEQ
                   PERFORM 375000-FORMATA-TRAILLER-ERROS.

           IF (GFCT00-HDR-ERRO-FISICO  OF  GFCTMVTA
                                       EQUAL    'S') AND
              (GFCT00-HDR-TP-REG       OF  GFCTMVTA
                                       NOT EQUAL  ZEROS AND 9)
               MOVE 'S'                TO  WRK-IND-ERRO-GRUPO
               PERFORM 340000-FORMATAR-TAB-GP.

           IF  GFCT00-HDR-TP-REG       OF GFCTMVTA
                                       NOT EQUAL 9
               PERFORM 350000-FAZER-CONSISTENCIAS
               PERFORM 360000-TESTAR-GRAVACAO
           END-IF.

           MOVE  GFCT00-HDR-TP-REG     OF GFCTMVTA
                                       TO WRK-TP-REG-ANT.

PR2801     IF  ACU-COMMIT    GREATER  47000
'              EXEC SQL
'                   COMMIT
'              END-EXEC
'              MOVE  ZEROS        TO  ACU-COMMIT
PR2801     END-IF.

           PERFORM 210000-LER-GFCTMVTA.

           IF  WRK-IND-FIM-ARQ-MOV     EQUAL     SPACES
               GO TO 300000-PROCESSAR.

           IF  IND-1                   GREATER ZEROS
                   MOVE ZEROS          TO IND-1
                   PERFORM 310000-DESCARREGAR-TAB-GP
           END-IF.

           IF  WRK-TP-REG-ANT          NOT EQUAL '9'
               PERFORM 370000-FORMATA-ARQ-CTRL-SEQ
               PERFORM 375000-FORMATA-TRAILLER-ERROS
           END-IF.

      *----------------------------------------------------------------*
       300000-99-FIM.                  EXIT.
      *----------------------------------------------------------------*
           EJECT
      *----------------------------------------------------------------*
       305000-GUARDA-CAMPOS            SECTION.
      *----------------------------------------------------------------*

           IF  GFCT00-HDR-TP-REG          OF GFCTMVTA
                                          EQUAL  0
               MOVE SPACES                TO GFCT01-HEADER
               INITIALIZE                 GFCT01-HEADER
               MOVE '00'                  TO GFCT01-HDR-TP-REG
               MOVE GFCT00-HDR-PERC-DEVOL OF GFCTMVTA
                                          TO WRK-PERC-DEVOLU
               MOVE GFCT00-HDR-CLIE-CRTT  OF GFCTMVTA
                                          TO WRK-CLIE-CRTT
                                             GFCT01-HDR-CLIE-CRTT
               MOVE GFCT00-HDR-TRATAMENTO OF GFCTMVTA
                                          TO GFCT01-HDR-TRATAMENTO
               MOVE GFCT00-HDR-ROT-ORIG   OF GFCTMVTA
                                          TO WRK-ROT-ORIG
                                             GFCT01-HDR-ORIG
               MOVE GFCT00-HDR-DATA-MOVTO OF GFCTMVTA
                                          TO WRK-DATA-MOVTO
                                             GFCT01-HDR-DATA-MOVTO
               MOVE GFCT00-HDR-SEQ-ARQ    OF GFCTMVTA
                                          TO WRK-SEQ-ARQ
                                             GFCT01-HDR-SEQ-ARQ
               MOVE GFCT00-HDR-TRATAMENTO OF GFCTMVTA
                                          TO WRK-TRATAMENTO
               PERFORM 312100-GRAVAR-GFCTMVER.

           IF  GFCT00-HDR-TP-REG       OF GFCTMVTA
                                       EQUAL  1
               MOVE GFCT00-EVN-AGE-ORIGEM  OF GFCTMVTA
                                           TO WRK-EVN-AGE-ORIG
               MOVE GFCT00-EVN-PAB-ORIGEM  OF GFCTMVTA
                                           TO WRK-EVN-PAB-ORIG
               MOVE GFCT00-EVN-AGE-DEB-CRED OF GFCTMVTA
                                           TO WRK-EVN-AGE-DEST
               MOVE GFCT00-EVN-PAB-DEB-CRED OF GFCTMVTA
                                           TO WRK-EVN-PAB-DEST
               MOVE GFCT00-EVN-CTA-DEB-CRED OF GFCTMVTA
                                           TO WRK-EVN-CTA-DEST
               MOVE GFCT00-EVN-IDENT-CLIE  OF GFCTMVTA
                                           TO WRK-IDENT-CLIE
               MOVE GFCT00-EVN-DATA-EVENTO OF GFCTMVTA
                                           TO WRK-DATA-EVN-AUX
               MOVE GFCT00-EVN-COD-TARIFA  OF GFCTMVTA
                                           TO WRK-EVN-COD-TARIFA
                                              WRK-AUX-COD-TARIFA
               MOVE GFCT00-EVN-CD-TARI-CORR-R  OF GFCTMVTA TO
                    WRK-CAMPO-TIRA-SINAL-17
               MOVE '+' TO WRK-CAMPO-TIRA-SINAL-S
               MOVE WRK-CAMPO-TIRA-SINAL-05 TO
                                              WRK-EVN-COD-TARIFA-CORR
                                              WRK-AUX-COD-TARIFA-CORR-R

               MOVE GFCT00-EVN-VALOR-PRINC OF GFCTMVTA
                                           TO WRK-EVN-VALOR-PRINC
               MOVE GFCT00-EVN-VALOR-TOT-SERVC
                                           OF GFCTMVTA
                                           TO WRK-EVN-VALOR-TOT-SERVC.

      *----------------------------------------------------------------*
       305000-99-FIM.                  EXIT.
      *----------------------------------------------------------------*
           EJECT
      *----------------------------------------------------------------*
       310000-DESCARREGAR-TAB-GP       SECTION.
      *----------------------------------------------------------------*

           ADD 1                       TO IND-1.

           IF  (IND-1                  GREATER 500) OR
               (WRK-TBGRP-CHAVE(IND-1) EQUAL     SPACES OR HIGH-VALUES)
               IF WRK-IND-ERRO-GRUPO   NOT EQUAL SPACES
                  PERFORM 312000-GRAVAR-GFCTMVRJ-2
               END-IF
FAIRES*        MOVE SPACES             TO WRK-TAB-GRUPO
FAIRES*        INITIALIZE WRK-TAB-GRUPO
FAIRES         MOVE WRK-TBGRP-I        TO WRK-TAB-GRUPO
               MOVE SPACES             TO WRK-IND-ERRO-GRUPO
               MOVE ZEROS              TO IND-1
               GO TO 310000-99-FIM
           END-IF.

           IF  WRK-IND-ERRO-GRUPO      EQUAL SPACES
               PERFORM 311000-GRAVAR-GFCTMVOK-2
               GO TO 310000-DESCARREGAR-TAB-GP
           END-IF.

           MOVE WRK-TBGRP-ERRO-FIS(IND-1) TO    WRK-IND-ERRO-FIS
           MOVE WRK-TBGRP-ERRO-LOG(IND-1) TO    WRK-IND-ERRO-LOG.

           IF  WRK-TBGRP-ERRO-FIS(IND-1) EQUAL SPACES AND
               WRK-TBGRP-ERRO-LOG(IND-1) EQUAL SPACES
               MOVE 'S'                TO WRK-TBGRP-ERRO-LOG(IND-1)
               MOVE  1092              TO WRK-TBGRP-COD-MSG(IND-1)
           END-IF.

           IF  WRK-TBGRP-ERRO-LOG(IND-1)
                                       EQUAL 'S'
               PERFORM 312000-FORMATAR-ARQ-ERROS.

           IF ( WRK-TBGRP-CHAVE(IND-1) NOT EQUAL
                                       WRK-TBGRP-CHAVE(IND-1 - 1) )
                                       AND
              ( IND-1                  GREATER        1 )
                PERFORM 312000-GRAVAR-GFCTMVRJ-2
           END-IF.

           MOVE SPACES                 TO WRK-IND-ERRO-FIS
                                          WRK-IND-ERRO-LOG

           GO TO 310000-DESCARREGAR-TAB-GP.

      *----------------------------------------------------------------*
       310000-99-FIM.                  EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       311000-GRAVAR-GFCTMVOK-2        SECTION.
      *----------------------------------------------------------------*

           MOVE WRK-TBGRP-REG(IND-1)   TO GFCT00-REG       OF GFCTMVOK.

           WRITE GFCT00-REG            OF GFCTMVOK.

           MOVE WRK-GRAVACAO           TO WRK-OPERACAO.

           PERFORM 112000-TESTAR-FS-GFCTMVOK.

           ADD 1                       TO WRK-CT-GRAV-OK.

           EVALUATE WRK-TBGRP-TP-REG(IND-1)
               WHEN ZEROS
                   ADD 1               TO WRK-CT-GRV-OK-0

               WHEN 1
                   ADD 1               TO WRK-CT-GRV-OK-1
                   ADD 1               TO WRK-CT-EVN-OK

               WHEN 2
                   ADD 1               TO WRK-CT-GRV-OK-2

               WHEN 3
                   ADD 1               TO WRK-CT-GRV-OK-3

               WHEN 4
                   ADD 1               TO WRK-CT-GRV-OK-4

               WHEN 9
                   ADD 1               TO WRK-CT-GRV-OK-9
           END-EVALUATE.

      *----------------------------------------------------------------*
       311000-99-FIM.                  EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       312000-FORMATAR-ARQ-ERROS       SECTION.
      *----------------------------------------------------------------*

           MOVE '01'                   TO GFCT01-HDR-TP-REG.
           MOVE WRK-TBGRP-CHAVE(IND-1) TO GFCT01-DET-CHAVE.
           MOVE WRK-TBGRP-COD-MSG(IND-1) TO GFCT01-DET-COD-MSG-ERRO.

           IF   WRK-TBGRP-TP-REG(IND-1)
                                       EQUAL   0 OR 9
                IF   WRK-TBGRP-TP-REG(IND-1)
                                       EQUAL   0
                     MOVE LOW-VALUES   TO      GFCT01-DET-IDENT-CLIE
                ELSE
                     MOVE HIGH-VALUES  TO      GFCT01-DET-IDENT-CLIE
                END-IF
           ELSE
                MOVE SPACES            TO GFCT01-DET-IDENT-CLIE
                IF   WRK-CLIE-CRTT     EQUAL         'S'
                     MOVE WRK-EVN-AGE-DEST   TO GFCT01-DET-AGENCIA
                     MOVE WRK-EVN-PAB-DEST   TO GFCT01-DET-PAB
                     MOVE WRK-EVN-CTA-DEST   TO GFCT01-DET-CONTA
                ELSE
                     MOVE WRK-IDENT-CLIE
                                       TO GFCT01-DET-IDENT-CLIE
                END-IF
           END-IF.

           MOVE WRK-DIA-EVN            TO GFCT01-DET-DIA-EVENTO.
           MOVE WRK-MES-EVN            TO GFCT01-DET-MES-EVENTO.
           MOVE WRK-ANO-EVN            TO GFCT01-DET-ANO-EVENTO.
           MOVE WRK-EVN-COD-TARIFA     TO GFCT01-DET-COD-TARIFA.
           MOVE WRK-EVN-COD-TARIFA-CORR TO GFCT01-DET-COD-TARIFA-CORR-R.
           MOVE SPACES                 TO GFCT01-DET-DSC-TARIFA.

           PERFORM 312100-GRAVAR-GFCTMVER.

           ADD 1                       TO WRK-CT-DEVOLVIDO.

      *----------------------------------------------------------------*
       312000-99-FIM.                  EXIT.
      *----------------------------------------------------------------*
           EJECT
      *----------------------------------------------------------------*
       312100-GRAVAR-GFCTMVER          SECTION.
      *----------------------------------------------------------------*

           WRITE GFCT01-REG            OF GFCTMVER.

           MOVE WRK-GRAVACAO           TO WRK-OPERACAO.

           PERFORM 114000-TESTAR-FS-GFCTMVER.

           ADD 1                       TO WRK-CT-GRAV-ERR.

      *----------------------------------------------------------------*
       312100-99-FIM.                  EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       312000-GRAVAR-GFCTMVRJ-2        SECTION.
      *----------------------------------------------------------------*

           MOVE WRK-IND-ERRO-FIS       TO WRK-TBGRP-ERRO-FIS(IND-1)
           MOVE WRK-IND-ERRO-LOG       TO WRK-TBGRP-ERRO-LOG(IND-1)

           MOVE WRK-TBGRP-REG(IND-1 - 1)
                                       TO GFCT00-REG  OF GFCTMVRJ.

           WRITE GFCT00-REG            OF GFCTMVRJ.

           MOVE WRK-GRAVACAO           TO WRK-OPERACAO.

           PERFORM 113000-TESTAR-FS-GFCTMVRJ.

           ADD 1                       TO WRK-CT-GRV-REJ.

           EVALUATE WRK-TBGRP-TP-REG(IND-1 - 1)
               WHEN ZEROS
                   ADD 1               TO WRK-CT-GRV-REJ-0

               WHEN 1
                   ADD 1               TO WRK-CT-GRV-REJ-1

               WHEN 2
                   ADD 1               TO WRK-CT-GRV-REJ-2

               WHEN 3
                   ADD 1               TO WRK-CT-GRV-REJ-3

               WHEN 4
                   ADD 1               TO WRK-CT-GRV-REJ-4

               WHEN 9
                   ADD 1               TO WRK-CT-GRV-REJ-9
           END-EVALUATE.

      *----------------------------------------------------------------*
       312000-99-FIM.                  EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       320000-GRAVAR-GFCTMVOK-1        SECTION.
      *----------------------------------------------------------------*

           MOVE GFCT00-REG             OF GFCTMVTA
                                       TO GFCT00-REG       OF GFCTMVOK.

           IF  GFCT00-TRL-TP-REG       OF GFCTMVTA
                                       EQUAL 9
               MOVE WRK-CT-LIDOS       TO GFCT00-TRL-QTDE-REG-ENV
                                                           OF GFCTMVOK
               MOVE WRK-CT-DEVOLVIDO   TO GFCT00-TRL-QTDE-REG-DEV
                                                           OF GFCTMVOK
               IF   WRK-CT-EVN-RECBD   GREATER             ZEROS
                    COMPUTE WRK-PERC-DEVOLVI = ((WRK-CT-EVN-RECBD -
                                                 WRK-CT-EVN-OK) /
                                                 WRK-CT-EVN-RECBD )
                                                 * 100
                    MOVE    WRK-PERC-DEVOLVI
                                       TO   GFCT00-TRL-PERC-DEVOLVIDO
                                            OF GFCTMVOK
               ELSE
                    MOVE    100        TO   GFCT00-TRL-PERC-DEVOLVIDO
                                            OF GFCTMVOK
               END-IF
               IF ((WRK-PERC-DEVOLVI    NOT LESS WRK-PERC-DEVOLU) AND
                   (WRK-PERC-DEVOLVI    NOT EQUAL          ZEROS) AND
                   (WRK-PERC-DEVOLU     NOT EQUAL          ZEROS)) OR
                   (WRK-PERC-DEVOLVI    EQUAL              100)
                   MOVE 'S'            TO WRK-INDARQ-REJ
                                          GFCT00-TRL-IND-ARQ-REJ
                                                           OF GFCTMVOK
               ELSE
                   MOVE WRK-INDARQ-REJ TO GFCT00-TRL-IND-ARQ-REJ
                                                           OF GFCTMVOK
               END-IF
               MOVE WRK-CT-EVN-RECBD   TO GFCT00-TRL-QTDE-EVN-RECBD
                                                           OF GFCTMVOK
               MOVE WRK-CT-EVN-OK      TO GFCT00-TRL-QTDE-EVN-OK
                                                           OF GFCTMVOK
           END-IF.

           WRITE GFCT00-REG            OF GFCTMVOK.

           MOVE WRK-GRAVACAO           TO WRK-OPERACAO.

           PERFORM 112000-TESTAR-FS-GFCTMVOK.

           ADD 1                       TO WRK-CT-GRAV-OK.

           EVALUATE GFCT00-TRL-TP-REG  OF GFCTMVTA
               WHEN ZEROS
                   ADD 1               TO WRK-CT-GRV-OK-0

               WHEN 1
                   ADD 1               TO WRK-CT-GRV-OK-1

               WHEN 2
                   ADD 1               TO WRK-CT-GRV-OK-2

               WHEN 3
                   ADD 1               TO WRK-CT-GRV-OK-3

               WHEN 4
                   ADD 1               TO WRK-CT-GRV-OK-4

               WHEN 9
                   ADD 1               TO WRK-CT-GRV-OK-9
           END-EVALUATE.

      *----------------------------------------------------------------*
       320000-99-FIM.                  EXIT.
      *----------------------------------------------------------------*
           EJECT
      *----------------------------------------------------------------*
       330000-GRAVAR-GFCTMVRJ-1        SECTION.
      *----------------------------------------------------------------*

           MOVE GFCT00-REG             OF GFCTMVTA
                                       TO GFCT00-REG       OF GFCTMVRJ.

           IF  GFCT00-TRL-TP-REG       OF GFCTMVTA
                                       EQUAL 9
               MOVE WRK-CT-LIDOS       TO GFCT00-TRL-QTDE-REG-ENV
                                                           OF GFCTMVRJ
               MOVE WRK-CT-DEVOLVIDO   TO GFCT00-TRL-QTDE-REG-DEV
                                                           OF GFCTMVRJ
               IF   WRK-CT-EVN-RECBD   GREATER             ZEROS
                    COMPUTE WRK-PERC-DEVOLVI = ((WRK-CT-EVN-RECBD -
                                                 WRK-CT-EVN-OK) /
                                                 WRK-CT-EVN-RECBD )
                                                 * 100
                    MOVE    WRK-PERC-DEVOLVI
                                       TO   GFCT00-TRL-PERC-DEVOLVIDO
                                            OF GFCTMVRJ
               ELSE
                    MOVE    100        TO   GFCT00-TRL-PERC-DEVOLVIDO
                                            OF GFCTMVRJ
               END-IF
               IF ((WRK-PERC-DEVOLVI    NOT LESS WRK-PERC-DEVOLU) AND
                   (WRK-PERC-DEVOLVI    NOT EQUAL          ZEROS) AND
                   (WRK-PERC-DEVOLU     NOT EQUAL          ZEROS)) OR
                   (WRK-PERC-DEVOLVI    EQUAL              100)
                   MOVE 'S'            TO GFCT00-TRL-IND-ARQ-REJ
                                                           OF GFCTMVRJ
               ELSE
                   MOVE WRK-INDARQ-REJ TO GFCT00-TRL-IND-ARQ-REJ
                                                           OF GFCTMVRJ
               END-IF
           END-IF.

           WRITE GFCT00-REG            OF GFCTMVRJ.

           MOVE WRK-GRAVACAO           TO WRK-OPERACAO.

           PERFORM 113000-TESTAR-FS-GFCTMVRJ.

           ADD 1                       TO WRK-CT-GRV-REJ.

           EVALUATE GFCT00-TRL-TP-REG  OF GFCTMVTA
               WHEN ZEROS
                   ADD 1               TO WRK-CT-GRV-REJ-0

               WHEN 1
                   ADD 1               TO WRK-CT-GRV-REJ-1

               WHEN 2
                   ADD 1               TO WRK-CT-GRV-REJ-2

               WHEN 3
                   ADD 1               TO WRK-CT-GRV-REJ-3

               WHEN 4
                   ADD 1               TO WRK-CT-GRV-REJ-4

               WHEN 9
                   ADD 1               TO WRK-CT-GRV-REJ-9
           END-EVALUATE.

      *----------------------------------------------------------------*
       330000-99-FIM.                  EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       340000-FORMATAR-TAB-GP          SECTION.
      *----------------------------------------------------------------*

           ADD 1                       TO IND-1.

           IF  IND-1                   GREATER 500
               MOVE 4                  TO RETURN-CODE
               MOVE WRK-MSG-TAB        TO ERR-TEXTO
               PERFORM 999999-PROCESSAR-ROTINA-ERRO
           END-IF

           MOVE GFCT00-REG             OF GFCTMVTA
                                       TO WRK-TBGRP-REG(IND-1).

           IF  WRK-TBGRP-ERRO-LOG(IND-1) EQUAL 'S'
               MOVE WRK-COD-MENSAGEM   TO WRK-TBGRP-COD-MSG(IND-1)
           END-IF.


      *----------------------------------------------------------------*
       340000-99-FIM.                  EXIT.
      *----------------------------------------------------------------*
           EJECT
      *----------------------------------------------------------------*
       350000-FAZER-CONSISTENCIAS      SECTION.
      *----------------------------------------------------------------*

           MOVE SPACES                 TO WRK-INDREG-REJ.

           EVALUATE GFCT00-HDR-TP-REG  OF GFCTMVTA
               WHEN ZEROS
                   PERFORM 351000-CONSISTIR-HEADER

               WHEN 1
                   PERFORM 352000-CONSISTIR-DET-EVENTO

               WHEN 2
                   PERFORM 353000-CONSISTIR-DET-DESCONTO
               WHEN 4
                   PERFORM 3540-CONSISTIR-DET-COMP
           END-EVALUATE.

      *----------------------------------------------------------------*
       350000-99-FIM.                  EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       351000-CONSISTIR-HEADER         SECTION.
      *----------------------------------------------------------------*

           PERFORM 351100-VALIDAR-ORIGEM.

           IF  WRK-IND-ERRO-HEADER     EQUAL 'S'
               MOVE 'S'                TO WRK-INDARQ-REJ
                                          WRK-INDREG-REJ
                                          GFCT00-HDR-ERRO-LOGICO
                                                           OF GFCTMVTA
               PERFORM 351200-FORMATAR-ARQ-ERROS-2
               MOVE SPACES             TO WRK-IND-ERRO-HEADER
           END-IF.


      *----------------------------------------------------------------*
       351000-99-FIM.                  EXIT.
      *----------------------------------------------------------------*
           EJECT
      *----------------------------------------------------------------*
       351100-VALIDAR-ORIGEM           SECTION.
      *----------------------------------------------------------------*

           MOVE SPACES                  TO  WRK-CROTNA-ORIGE.

AT0522     INITIALIZE                       CARO01-COMUAREA.
AT0522     MOVE 'S'                     TO  CARO01-ACAO.
AT0522     MOVE 01                      TO  CARO01-TIPO-PESQUISA.
           MOVE GFCT00-HDR-ROT-ORIG     OF  GFCTMVTA
AT0522                                  TO  WRK-CCTRO-CUSTO.
AT0522     MOVE WRK-CCTRO-CUSTO-R       TO  CARO01-CCTRO-CUSTO.

AT0522     CALL  WRK-CARO8200 USING CARO01-COMUAREA.

AT0522     IF (CARO01-COD-RETORNO      NOT EQUAL ZEROS AND 01  )
               MOVE 8                    TO RETURN-CODE
               MOVE 'DB2'                TO ERR-TIPO-ACESSO
AT0522         MOVE 'TCARO_ATIV      '   TO ERR-DBD-TAB
AT0522         MOVE 'CARO8200  '         TO ERR-FUN-COMANDO
AT0522         MOVE  CARO01-ERRO-SQLCODE TO ERR-STA-CODE
               MOVE  '0010'              TO ERR-LOCAL
               MOVE  SPACES              TO ERR-SEGM
               PERFORM 999999-PROCESSAR-ROTINA-ERRO
           END-IF.

AT0522     IF  CARO01-COD-RETORNO      EQUAL 01
               MOVE 'S'                TO WRK-IND-ERRO-HEADER
               MOVE  1079              TO WRK-COD-MENSAGEM
           ELSE
               MOVE GFCT00-HDR-ROT-ORIG OF GFCTMVTA
                                       TO WRK-CROTNA-ORIGE
           END-IF.

      *----------------------------------------------------------------*
       351100-99-FIM.                  EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       351200-FORMATAR-ARQ-ERROS-2     SECTION.
      *----------------------------------------------------------------*

           MOVE GFCT00-HDR-CHAVE       OF GFCTMVTA
                                       TO GFCT01-DET-CHAVE.
           MOVE WRK-COD-MENSAGEM       TO GFCT01-DET-COD-MSG-ERRO.
           MOVE ZEROS                  TO GFCT01-DET-DIA-EVENTO
                                          GFCT01-DET-MES-EVENTO
                                          GFCT01-DET-ANO-EVENTO
                                          GFCT01-DET-AGENCIA
                                          GFCT01-DET-PAB
                                          GFCT01-DET-CONTA.

           MOVE SPACES                 TO GFCT01-DET-IDENT-CLIE
                                          GFCT01-DET-COD-TARIFA
                                          GFCT01-DET-COD-TARIFA-CORR
                                          GFCT01-DET-DSC-TARIFA.

           MOVE '01'                   TO GFCT01-DET-TP-REG.

           PERFORM 312100-GRAVAR-GFCTMVER.

           ADD 1                       TO WRK-CT-DEVOLVIDO.

      *----------------------------------------------------------------*
       351200-99-FIM.                  EXIT.
      *----------------------------------------------------------------*
           EJECT

      *----------------------------------------------------------------*
       3519-VALIDA-ANO-NA-K7           SECTION.
      *----------------------------------------------------------------*

           MOVE  ' '                   TO  WRK-IND-ERRO-DETALHE.

           MOVE GFCT00-ENV-ANO-REFER   OF  GFCTMVTA
                                       TO  DANO-REFT-EXTRT
                                           OF   GFCTB0K7.

           EXEC SQL
                SELECT DANO_REFT_EXTRT
                INTO  :GFCTB0K7.DANO-REFT-EXTRT
                FROM   DB2PRD.THOMOL_EXTRT_TARIF
                WHERE  DANO_REFT_EXTRT = :GFCTB0K7.DANO-REFT-EXTRT
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS AND +100
                                                       AND -811 ) OR
              (SQLWARN0                EQUAL 'W' )
               MOVE 8                  TO RETURN-CODE
               MOVE 'DB2'              TO ERR-TIPO-ACESSO
               MOVE 'GFCTB0K7'         TO ERR-DBD-TAB
               MOVE 'SELECT'           TO ERR-FUN-COMANDO
               MOVE  SQLCODE           TO ERR-SQL-CODE
               MOVE '0120'             TO ERR-LOCAL
               MOVE  SPACES            TO ERR-SEGM
               PERFORM 999999-PROCESSAR-ROTINA-ERRO
           END-IF.

           IF  SQLCODE                 EQUAL   +100
               MOVE  'S'               TO      WRK-IND-ERRO-DETALHE
           END-IF.

      *----------------------------------------------------------------*
       3519-99-FIM.                    EXIT.
      *----------------------------------------------------------------*
           EJECT

      *----------------------------------------------------------------*
       352000-CONSISTIR-DET-EVENTO     SECTION.
      *----------------------------------------------------------------*

           PERFORM 352100-VALIDAR-COD-TARIFA.

ATEND      IF  WRK-5005-COD-RETORNO EQUAL ZEROS OR 3
               PERFORM 352910-OBTER-DADOS-TARIFA
           END-IF.

           IF  WRK-IND-ERRO-DETALHE    EQUAL 'S'
               MOVE 'S'                TO WRK-INDREG-REJ
                                          WRK-IND-ERRO-GRUPO
                                          GFCT00-HDR-ERRO-LOGICO
                                                           OF GFCTMVTA
               PERFORM 340000-FORMATAR-TAB-GP
               MOVE ZEROS              TO WRK-AUX-COD-TARIFA
                                          WRK-AUX-COD-TARIFA-CORR-R
               MOVE SPACES             TO WRK-IND-ERRO-DETALHE
           END-IF.

           PERFORM 352900-CHECA-VALOR-TARIFA

           IF  WRK-IND-TAR-MOEDA-EVENTO    EQUAL     'S'   AND
               WRK-COD-RET-NTAX        NOT EQUAL     ZEROS
               IF  WRK-COD-RET-NTAX        EQUAL     2 OR 7
                   MOVE  1123          TO  WRK-COD-MENSAGEM
               ELSE
                   MOVE  1124          TO  WRK-COD-MENSAGEM
               END-IF

               MOVE 'S'                TO  WRK-IND-ERRO-TPCTA
                                           WRK-INDREG-REJ
                                           WRK-IND-ERRO-GRUPO
                                           GFCT00-HDR-ERRO-LOGICO
                                                      OF GFCTMVTA

               PERFORM 340000-FORMATAR-TAB-GP
           END-IF.

           PERFORM 352200-VALIDAR-MIDIA.

           IF  WRK-IND-ERRO-DETALHE    EQUAL 'S'
               MOVE 'S'                TO WRK-IND-ERRO-MIDIA
               MOVE 'S'                TO WRK-INDREG-REJ
                                          WRK-IND-ERRO-GRUPO
                                          GFCT00-HDR-ERRO-LOGICO
                                                           OF GFCTMVTA

               PERFORM 340000-FORMATAR-TAB-GP

               MOVE SPACES                TO WRK-IND-ERRO-DETALHE
           END-IF.

           IF (GFCT00-EVN-TARIFA-PERIF OF GFCTMVTA NUMERIC)    AND
              (GFCT00-EVN-TARIFA-PERIF OF GFCTMVTA NOT EQUAL ZEROS)
               PERFORM 352300-VALIDAR-PERIFERICO

               IF WRK-IND-ERRO-DETALHE EQUAL 'S'
                  MOVE 'S'             TO WRK-IND-ERRO-PERIF
                  MOVE 'S'             TO WRK-INDREG-REJ
                                          WRK-IND-ERRO-GRUPO
                                          GFCT00-HDR-ERRO-LOGICO
                                          OF GFCTMVTA

                  PERFORM 340000-FORMATAR-TAB-GP

                  MOVE SPACES          TO WRK-IND-ERRO-DETALHE
               END-IF
           END-IF.

           IF  GFCT00-EVN-TARIFA-PERIF OF GFCTMVTA NOT NUMERIC
               MOVE 'S'                   TO WRK-IND-ERRO-PERIF
               MOVE  1142                 TO WRK-COD-MENSAGEM
               MOVE 'S'                   TO WRK-IND-ERRO-TPCTA
                                             WRK-INDREG-REJ
                                             WRK-IND-ERRO-GRUPO
                                             GFCT00-HDR-ERRO-LOGICO
                                                        OF GFCTMVTA

               PERFORM 340000-FORMATAR-TAB-GP
           END-IF.

      *--- SE MIDIA OU PERIFERICO INVALIDOS NAO VALIDA ASSOCIACAO
           IF  GFCT00-EVN-TARIFA-PERIF OF GFCTMVTA NOT EQUAL ZEROS
               IF (WRK-IND-ERRO-MIDIA         EQUAL 'S') OR
                  (WRK-IND-ERRO-PERIF         EQUAL 'S')
                   MOVE  1143                 TO WRK-COD-MENSAGEM
                   MOVE 'S'                   TO WRK-IND-ERRO-TPCTA
                                                 WRK-INDREG-REJ
                                                 WRK-IND-ERRO-GRUPO
                                                 GFCT00-HDR-ERRO-LOGICO
                                                 OF GFCTMVTA

                   PERFORM 340000-FORMATAR-TAB-GP

                   MOVE SPACES            TO     WRK-IND-ERRO-DETALHE
                                                 WRK-IND-ERRO-MIDIA
                                                 WRK-IND-ERRO-PERIF
               ELSE
                   PERFORM 352350-OBTER-ASSOCIA-MID-PER

                   IF SQLCODE             EQUAL  ZEROS
                      MOVE CTPO-MIDIA     OF     GFCTB0G8
                                          TO     GFCT00-EVN-TARIFA-MIDIA
                                                 OF GFCTMVTA
                   END-IF
               END-IF
           END-IF.

      *    PERFORM 352400-VALIDAR-BANCO

      *    IF  WRK-IND-ERRO-DETALHE    EQUAL 'S'
      *        MOVE 'S'                TO WRK-INDREG-REJ
      *                                   WRK-IND-ERRO-GRUPO
      *                                   GFCT00-HDR-ERRO-LOGICO
      *                                                    OF GFCTMVTA
      *        MOVE  1075              TO WRK-COD-MENSAGEM
      *        PERFORM 340000-FORMATAR-TAB-GP
      *        MOVE SPACES             TO WRK-IND-ERRO-DETALHE
      *    END-IF.

           PERFORM 352500-VALIDAR-AGENCIA

           IF  WRK-IND-ERRO-DETALHE    EQUAL 'S'
               MOVE 'S'                TO WRK-INDREG-REJ
                                          WRK-IND-ERRO-GRUPO
                                          GFCT00-HDR-ERRO-LOGICO
                                                           OF GFCTMVTA

               PERFORM 340000-FORMATAR-TAB-GP

               MOVE SPACES             TO WRK-IND-ERRO-DETALHE
           END-IF.

           IF  GFCT00-EVN-PAB-ORIGEM   OF  GFCTMVTA
                                       NOT EQUAL       ZEROS
               PERFORM 352600-VALIDAR-PAB-ORIGEM.

           IF  WRK-IND-ERRO-DETALHE    EQUAL 'S'
               MOVE 'S'                TO WRK-INDREG-REJ
                                          WRK-IND-ERRO-GRUPO
                                          GFCT00-HDR-ERRO-LOGICO
                                                           OF GFCTMVTA

               PERFORM 340000-FORMATAR-TAB-GP

               MOVE SPACES             TO WRK-IND-ERRO-DETALHE
           END-IF.

           IF  WRK-CLIE-CRTT           EQUAL 'S' AND
               GFCT00-EVN-BCO-DEB-CRED OF GFCTMVTA
                                       NOT EQUAL 237
               MOVE 'S'                TO WRK-INDREG-REJ
                                          WRK-IND-ERRO-GRUPO
                                          GFCT00-HDR-ERRO-LOGICO
                                                           OF GFCTMVTA
               MOVE  1076              TO WRK-COD-MENSAGEM

               PERFORM 340000-FORMATAR-TAB-GP
           END-IF.

           MOVE 'N' TO WRK-TIPO-CONTA.

           IF  WRK-CLIE-CRTT           EQUAL    'S'
               MOVE  'N'               TO       WRK-IND-ERRO-TPCTA
               MOVE  SPACES            TO       WRK-CID-CLI

               IF GFCT00-EVN-CTA-DEB-CRED OF  GFCTMVTA
                                       LESS   1000000
                  PERFORM 352700-VALIDAR-AGE-CONTA-CLIE
               ELSE
                  PERFORM 352750-VALIDAR-AGE-CTA-CLIE-P
               END-IF

               IF  WRK-CID-CLI         NOT EQUAL   SPACES
                   PERFORM 352760-VALIDAR-TIPO-PESSOA
                   IF  WRK-IND-ERRO-DETALHE  EQUAL   'S'
                       MOVE 'S'              TO WRK-INDREG-REJ
                                                WRK-IND-ERRO-GRUPO
                                                GFCT00-HDR-ERRO-LOGICO
                                                           OF GFCTMVTA
                       PERFORM 340000-FORMATAR-TAB-GP
                       MOVE SPACES           TO WRK-IND-ERRO-DETALHE
                   END-IF
               END-IF

               IF  (WRK-5318-CTPO-CTA-DEB-TARIF  EQUAL 1 AND
                    WRK-TIPO-CONTA EQUAL 'C')                  OR
CI0815             (WRK-5318-CTPO-CTA-DEB-TARIF  EQUAL 2 AND
CI0815              WRK-TIPO-CONTA EQUAL 'F')                  OR
                   (WRK-5318-CTPO-CTA-DEB-TARIF  EQUAL 2 AND
                    WRK-TIPO-CONTA EQUAL 'P')                  OR
                   (WRK-5318-CTPO-CTA-DEB-TARIF  EQUAL 3 AND
                    WRK-TIPO-CONTA EQUAL 'I')
______*_____________NEXT_SENTENCE
                    MOVE WRK-TIPO-CONTA  TO WRK-TIPO-CONTA
                ELSE
                    MOVE 'S'            TO       WRK-INDREG-REJ
                                                WRK-IND-ERRO-GRUPO
                                                GFCT00-HDR-ERRO-LOGICO
                                                OF GFCTMVTA
                   MOVE  1036          TO       WRK-COD-MENSAGEM

                   PERFORM 340000-FORMATAR-TAB-GP
                END-IF

               IF  (WRK-5318-CTPO-CTA-DEB-TARIF  NOT EQUAL
                    GFCT00-EVN-TIPO-CONTA        OF      GFCTMVTA)
                    MOVE 'S'            TO       WRK-INDREG-REJ
                                                 WRK-IND-ERRO-GRUPO
                                                GFCT00-HDR-ERRO-LOGICO
                                                OF GFCTMVTA
                    MOVE  1118          TO       WRK-COD-MENSAGEM
                    PERFORM 340000-FORMATAR-TAB-GP
               END-IF

               IF (GFCT00-EVN-TIPO-CONTA        OF      GFCTMVTA
                                                EQUAL   1)        AND
                  (GFCT00-EVN-CTA-DEB-CRED      OF      GFCTMVTA
                                                NOT LESS 1000000) AND
                  (WRK-IND-ERRO-DETALHE-CLIE    NOT EQUAL 'S')
                   MOVE 'S'            TO       WRK-INDREG-REJ
                                                WRK-IND-ERRO-GRUPO
                                                GFCT00-HDR-ERRO-LOGICO
                                                OF  GFCTMVTA
                   MOVE  1119          TO       WRK-COD-MENSAGEM

                   PERFORM 340000-FORMATAR-TAB-GP
               END-IF

               IF  WRK-TRATAMENTO            NOT EQUAL 05 AND 99
                   PERFORM 353500-VALIDAR-CONTA-EM-SCCE

                   IF  WRK-IND-ERRO-DETALHE  EQUAL   'S'
                       MOVE 'S'              TO WRK-INDREG-REJ
                                                WRK-IND-ERRO-GRUPO
                                                GFCT00-HDR-ERRO-LOGICO
                                                           OF GFCTMVTA

                       PERFORM 340000-FORMATAR-TAB-GP

                       MOVE SPACES           TO WRK-IND-ERRO-DETALHE
                   END-IF
               END-IF

               IF  WRK-IND-ERRO-TPCTA         EQUAL   'N'
                   IF  WRK-IND-ERRO-DETALHE-CLIE       EQUAL 'S'
                       MOVE 'S'            TO WRK-INDREG-REJ
                                              WRK-IND-ERRO-GRUPO
                                              GFCT00-HDR-ERRO-LOGICO
                                                         OF GFCTMVTA
                       MOVE  1077          TO WRK-COD-MENSAGEM
                       PERFORM 340000-FORMATAR-TAB-GP
                       MOVE SPACES         TO WRK-IND-ERRO-DETALHE-CLIE
                   ELSE
                       IF  WRK-POSTO-SERVC = ZEROS
                           MOVE ZEROS         TO
                           GFCT00-EVN-PAB-DEB-CRED OF GFCTMVTA
                       ELSE
                           PERFORM  352650-VALIDAR-PAB-DEB-CRED
                           IF  COMU-TEXTO-MSG(1:3) EQUAL  '001'
                               MOVE WRK-POSTO-SERVC   TO
                               GFCT00-EVN-PAB-DEB-CRED OF GFCTMVTA
                           ELSE
                               MOVE ZEROS     TO
                               GFCT00-EVN-PAB-DEB-CRED OF GFCTMVTA
                           END-IF
                       END-IF
                   END-IF
               END-IF
               MOVE        'N'             TO WRK-IND-ERRO-DETALHE
           END-IF.

           IF  GFCT00-EVN-TIPO-MOEDA   OF  GFCTMVTA
                                       NOT EQUAL ZEROS
               PERFORM  352800-VALIDAR-MOEDA
               IF  WRK-IND-ERRO-DETALHE EQUAL 'S'
                   MOVE 'S'            TO WRK-INDREG-REJ
                                          WRK-IND-ERRO-GRUPO
                                          GFCT00-HDR-ERRO-LOGICO
                                                           OF GFCTMVTA
                   PERFORM 340000-FORMATAR-TAB-GP
                   MOVE SPACES         TO WRK-IND-ERRO-DETALHE
               END-IF
           END-IF.

           MOVE GFCT00-EVN-DATA-DEB-CRED  OF GFCTMVTA
                                          TO     WRK-DATA-DEBITO.
           MOVE WRK-DIA-DEB               TO     WRK-DIA-DEB8
           MOVE WRK-MES-DEB               TO     WRK-MES-DEB8
           MOVE WRK-ANO-DEB               TO     WRK-ANO-DEB8.

           MOVE GFCT00-EVN-DATA-EVENTO    OF     GFCTMVTA
                                          TO     WRK-DATA-AUX.
           MOVE WRK-ANO-AUX               TO     WRK-ANO-EVENTO
           MOVE WRK-MES-AUX               TO     WRK-MES-EVENTO
           MOVE WRK-DIA-AUX               TO     WRK-DIA-EVENTO.

           IF   WRK-DATA-EVENTO           GREATER   WRK-DPROCM-PROX
                MOVE 'S'                  TO     WRK-INDREG-REJ
                                                 WRK-IND-ERRO-GRUPO
                                                 GFCT00-HDR-ERRO-LOGICO
                                                 OF GFCTMVTA
                MOVE 1121                 TO     WRK-COD-MENSAGEM
                PERFORM 340000-FORMATAR-TAB-GP
           END-IF.

      ***  IF  (WRK-TRATAMENTO            NOT EQUAL 05 AND 99)    AND
      ***      (WRK-DATA-DEB-8            LESS   WRK-DPROCM-PROX) AND
      ***      (GFCT00-EVN-DATA-DEB-CRED  OF     GFCTMVTA
      ***                             NOT EQUAL  SPACES)
      ***       MOVE 'S'                  TO     WRK-INDREG-REJ
      ***                                        WRK-IND-ERRO-GRUPO
      ***                                        GFCT00-HDR-ERRO-LOGICO
      ***                                                   OF GFCTMVTA
      ***       MOVE 1139                 TO     WRK-COD-MENSAGEM
      ***       PERFORM 340000-FORMATAR-TAB-GP
      ***  END-IF.

           IF    WRK-DATA-DEB-8             NUMERIC
             IF (WRK-TRATAMENTO             NOT  EQUAL 01 AND 02) AND
                (WRK-DATA-DEB-8             LESS WRK-DATA-EVENTO) AND
                (GFCT00-EVN-DATA-DEB-CRED   OF   GFCTMVTA
                                            NOT EQUAL  SPACES)
                MOVE 'S'                    TO   WRK-INDREG-REJ
                                                 WRK-IND-ERRO-GRUPO
                                                 GFCT00-HDR-ERRO-LOGICO
                                                 OF GFCTMVTA
                MOVE 1096                   TO   WRK-COD-MENSAGEM
                PERFORM 340000-FORMATAR-TAB-GP
             END-IF
           END-IF.

           IF  GFCT00-EVN-VALOR-EVENTO    OF     GFCTMVTA
                                          EQUAL  ZEROS
               IF WRK-TEM-PERCENTUAL      EQUAL  'S'
                  MOVE 'S'                TO     WRK-INDREG-REJ
                                                 WRK-IND-ERRO-GRUPO
                                                 GFCT00-HDR-ERRO-LOGICO
                                                 OF GFCTMVTA
                  MOVE  1097              TO     WRK-COD-MENSAGEM
                  PERFORM 340000-FORMATAR-TAB-GP
               END-IF
           END-IF.

           IF (GFCT00-EVN-TIPO-MOEDA   OF   GFCTMVTA
                                       NOT  EQUAL ZEROS)  AND
              (GFCT00-EVN-DATA-CONV    OF   GFCTMVTA
                                       NOT  EQUAL SPACES)
               MOVE  GFCT00-EVN-DATA-CONV   OF    GFCTMVTA
                                            TO    WRK-DATA-AUX
               MOVE  WRK-ANO-AUX            TO    WRK-ANO-CONV8
               MOVE  WRK-MES-AUX            TO    WRK-MES-CONV8
               MOVE  WRK-DIA-AUX            TO    WRK-DIA-CONV8

               IF    WRK-DATA-CONV-8        LESS  WRK-DATA-EVENTO
                     MOVE 'S'               TO    WRK-INDREG-REJ
                                                  WRK-IND-ERRO-GRUPO
                                                  GFCT00-HDR-ERRO-LOGICO
                                                  OF GFCTMVTA
                     MOVE  1098             TO    WRK-COD-MENSAGEM
                     PERFORM 340000-FORMATAR-TAB-GP
               END-IF
           END-IF.

           IF  WRK-5318-CINDCD-EVNTO-PCELD  EQUAL      'S'
               PERFORM   352050-CONSISTIR-EVNTO-PARCEL
           END-IF.

           IF  WRK-5318-CINDCD-QTD-REG      EQUAL    'S'  AND
               GFCT00-EVN-QTD-REGS          OF  GFCTMVTA  NOT NUMERIC
               MOVE 'S'                 TO  WRK-INDREG-REJ
                                            WRK-IND-ERRO-GRUPO
                                            GFCT00-HDR-ERRO-LOGICO
                                            OF GFCTMVTA
               MOVE  1145               TO  WRK-COD-MENSAGEM
               PERFORM 340000-FORMATAR-TAB-GP
           END-IF.

           IF  WRK-5318-CINDCD-NRO-DOCTO    EQUAL    'S'  AND
               GFCT00-NUM-DOCUMENTO         OF GFCTMVTA NOT NUMERIC
               MOVE 'S'                 TO  WRK-INDREG-REJ
                                            WRK-IND-ERRO-GRUPO
                                            GFCT00-HDR-ERRO-LOGICO
                                            OF GFCTMVTA
               MOVE  1147               TO  WRK-COD-MENSAGEM
               PERFORM 340000-FORMATAR-TAB-GP
           END-IF.

           IF  WRK-5318-CPER-FRANQ-TARIF    EQUAL     5
               IF  GFCT00-ENV-ANO-REFER     OF GFCTMVTA NUMERIC   AND
                   GFCT00-ENV-ANO-REFER     OF GFCTMVTA GREATER ZEROS
                   PERFORM 3519-VALIDA-ANO-NA-K7
                   IF  WRK-IND-ERRO-DETALHE EQUAL 'S'
                       MOVE 'S'             TO WRK-INDREG-REJ
                                               WRK-IND-ERRO-GRUPO
                                               GFCT00-HDR-ERRO-LOGICO
                                                           OF GFCTMVTA
                       MOVE 1248            TO WRK-COD-MENSAGEM
                       PERFORM 340000-FORMATAR-TAB-GP
                       MOVE SPACES          TO WRK-IND-ERRO-DETALHE
                   END-IF
               ELSE
                       MOVE 'S'             TO WRK-INDREG-REJ
                                               WRK-IND-ERRO-GRUPO
                                               GFCT00-HDR-ERRO-LOGICO
                                                           OF GFCTMVTA
                       MOVE 1249            TO WRK-COD-MENSAGEM
                       PERFORM 340000-FORMATAR-TAB-GP
                       MOVE SPACES          TO WRK-IND-ERRO-DETALHE
               END-IF
           END-IF.

      *----------------------------------------------------------------*
       352000-99-FIM.                  EXIT.
      *----------------------------------------------------------------*
           EJECT
      *----------------------------------------------------------------*
       352050-CONSISTIR-EVNTO-PARCEL   SECTION.
      *----------------------------------------------------------------*

           IF (GFCT00-EVN-NUM-PARCELA           OF    GFCTMVTA
                                                      NUMERIC)  AND
              (GFCT00-EVN-QTD-TOT-PARC          OF    GFCTMVTA
                                                      NUMERIC)  AND
              (GFCT00-EVN-VALOR-TOT-SERVC       OF    GFCTMVTA
                                                      NUMERIC)
               IF (GFCT00-EVN-NUM-PARCELA       OF    GFCTMVTA
                                                EQUAL ZEROS)  AND
                  (GFCT00-EVN-QTD-TOT-PARC      OF    GFCTMVTA
                                                EQUAL ZEROS)  AND
                  (GFCT00-EVN-VALOR-TOT-SERVC   OF    GFCTMVTA
                                                EQUAL ZEROS)
                   MOVE 'N'                     TO    WRK-EVNTO-PARC
                   GO TO 352050-99-FIM
               ELSE
                   MOVE 'S'                     TO    WRK-EVNTO-PARC
                   GO TO 352050-TRATA-PARCELADOS
               END-IF
           END-IF.

           IF (GFCT00-EVN-NUM-PARCELA       OF  GFCTMVTA
                                            NOT NUMERIC)  AND
              (GFCT00-EVN-QTD-TOT-PARC      OF  GFCTMVTA
                                            NOT NUMERIC)  AND
              (GFCT00-EVN-VALOR-TOT-SERVC   OF  GFCTMVTA
                                            NOT NUMERIC)
               MOVE 'N'                     TO  WRK-EVNTO-PARC
               GO TO 352050-99-FIM
           END-IF.

      *-- IDENTIFICA QUAIS CAMPOS PARCELA NAO NUMERICOS E REJEITA

           MOVE 'S'                   TO    WRK-EVNTO-PARC.

           IF  GFCT00-EVN-NUM-PARCELA       OF GFCTMVTA
                                            NOT NUMERIC
               MOVE 'S'               TO    WRK-INDREG-REJ
                                            WRK-IND-ERRO-GRUPO
                                            GFCT00-HDR-ERRO-LOGICO
                                            OF GFCTMVTA
               MOVE 1125              TO    WRK-COD-MENSAGEM
               PERFORM 340000-FORMATAR-TAB-GP
           END-IF.

           IF  GFCT00-EVN-QTD-TOT-PARC      OF GFCTMVTA
                                            NOT NUMERIC
               MOVE 'S'               TO    WRK-INDREG-REJ
                                            WRK-IND-ERRO-GRUPO
                                            GFCT00-HDR-ERRO-LOGICO
                                            OF GFCTMVTA
               MOVE 1126              TO    WRK-COD-MENSAGEM
               PERFORM 340000-FORMATAR-TAB-GP
           END-IF.

           IF  GFCT00-EVN-VALOR-TOT-SERVC   OF GFCTMVTA
                                            NOT NUMERIC
               MOVE 'S'               TO    WRK-INDREG-REJ
                                            WRK-IND-ERRO-GRUPO
                                            GFCT00-HDR-ERRO-LOGICO
                                            OF GFCTMVTA
               MOVE 1127              TO    WRK-COD-MENSAGEM
               PERFORM 340000-FORMATAR-TAB-GP
           END-IF.

           GO TO 352050-99-FIM.

      *-- CONSISTE EVENTOS IDENTIFICADOS COMO PARCELADOS.

       352050-TRATA-PARCELADOS.

           IF  GFCT00-EVN-NUM-PARCELA       OF GFCTMVTA
                                            NOT GREATER ZEROS
               MOVE 'S'               TO    WRK-INDREG-REJ
                                            WRK-IND-ERRO-GRUPO
                                            GFCT00-HDR-ERRO-LOGICO
                                            OF GFCTMVTA
               MOVE 1128              TO    WRK-COD-MENSAGEM
               PERFORM 340000-FORMATAR-TAB-GP
           END-IF.

           IF  GFCT00-EVN-QTD-TOT-PARC      OF GFCTMVTA
                                            NOT GREATER ZEROS
               MOVE 'S'               TO    WRK-INDREG-REJ
                                            WRK-IND-ERRO-GRUPO
                                            GFCT00-HDR-ERRO-LOGICO
                                            OF GFCTMVTA
               MOVE 1129              TO    WRK-COD-MENSAGEM
               PERFORM 340000-FORMATAR-TAB-GP
           END-IF.

           IF  GFCT00-EVN-VALOR-TOT-SERVC   OF GFCTMVTA
                                            NOT GREATER ZEROS
               MOVE 'S'               TO    WRK-INDREG-REJ
                                            WRK-IND-ERRO-GRUPO
                                            GFCT00-HDR-ERRO-LOGICO
                                            OF GFCTMVTA
               MOVE 1130              TO    WRK-COD-MENSAGEM
               PERFORM 340000-FORMATAR-TAB-GP
           END-IF.

           IF  GFCT00-EVN-NUM-PARCELA OF    GFCTMVTA
                                      GREATER
                                      GFCT00-EVN-QTD-TOT-PARC
                                      OF    GFCTMVTA
               MOVE 'S'               TO    WRK-INDREG-REJ
                                            WRK-IND-ERRO-GRUPO
                                            GFCT00-HDR-ERRO-LOGICO
                                            OF GFCTMVTA
               MOVE 1131              TO    WRK-COD-MENSAGEM
               PERFORM 340000-FORMATAR-TAB-GP
           END-IF.


           IF  GFCT00-EVN-VALOR-TOT-SERVC   OF GFCTMVTA
                                            LESS
                                            GFCT00-EVN-VALOR-PRINC
                                            OF GFCTMVTA
               MOVE 'S'               TO    WRK-INDREG-REJ
                                            WRK-IND-ERRO-GRUPO
                                            GFCT00-HDR-ERRO-LOGICO
                                            OF GFCTMVTA
               MOVE 1134              TO    WRK-COD-MENSAGEM
               PERFORM 340000-FORMATAR-TAB-GP
           END-IF.

      *----------------------------------------------------------------*
       352050-99-FIM.                  EXIT.
      *----------------------------------------------------------------*
           EJECT
      *----------------------------------------------------------------*
       352100-VALIDAR-COD-TARIFA       SECTION.
      *----------------------------------------------------------------*

           MOVE 'B'                    TO WRK-5005-AMBIENTE.
           IF GFCT00-EVN-CD-TARI-CORR-R OF GFCTMVTA EQUAL ZEROS
              MOVE GFCT00-EVN-COD-TARIFA  OF GFCTMVTA TO
                   WRK-CAMPO-TIRA-SINAL-17
              MOVE '+' TO WRK-CAMPO-TIRA-SINAL-S
              MOVE WRK-CAMPO-TIRA-SINAL-05 TO WRK-5005-CODIGO-TARIFA
           ELSE
              MOVE GFCT00-EVN-CD-TARI-CORR-R  OF GFCTMVTA TO
                   WRK-CAMPO-TIRA-SINAL-17
              MOVE '+' TO WRK-CAMPO-TIRA-SINAL-S
              MOVE WRK-CAMPO-TIRA-SINAL-05 TO WRK-5005-CODIGO-TARIFA
           END-IF

           MOVE GFCT00-EVN-DATA-EVENTO OF GFCTMVTA
                                       TO WRK-DATA-VIGENCIA.

           MOVE WRK-DIA-VIG            TO WRK-DIA-DB2
           MOVE WRK-MES-VIG            TO WRK-MES-DB2
           MOVE WRK-ANO-VIG            TO WRK-ANO-DB2.

           MOVE WRK-DATA-DB2           TO WRK-5005-DATA-INI-VIGEN.


FAIRES     IF ((WRK-5005-DATA-INI-VIGEN NOT EQUAL WRK-DT-INI-VIGEN-ANT)
  "                                     OR
  "            (WRK-5005-CODIGO-TARIFA  NOT EQUAL WRK-CODIGO-TARIF-ANT))
  "             MOVE WRK-5005-DATA-INI-VIGEN  TO  WRK-DT-INI-VIGEN-ANT
  "             MOVE WRK-5005-CODIGO-TARIFA   TO  WRK-CODIGO-TARIF-ANT
  "             MOVE 'GFCT5005'         TO   WRK-MODULO
  "
  "             CALL WRK-MODULO        USING WRK-5005-AREA-ENTRADA
  "                                          WRK-5005-AREA-SAIDA
  "                                          WRK-IO-PCB
  "                                          WRK-ALT-PCB
  "             IF  WRK-5005-COD-RETORNO    NOT EQUAL ZEROS AND 2 AND 3
  "                 MOVE 8                  TO RETURN-CODE
  "                 MOVE WRK-5005-COD-RETORNO
  "                                         TO WRK-COD-RET-5005
  "                 MOVE WRK-MSG-5005       TO ERR-TEXTO
  "                 PERFORM 999999-PROCESSAR-ROTINA-ERRO
  "             END-IF
FAIRES     END-IF.

           IF  WRK-5005-COD-RETORNO EQUAL 2
               MOVE 'S'            TO WRK-IND-ERRO-DETALHE
               MOVE  1080          TO WRK-COD-MENSAGEM
           END-IF.

      *----------------------------------------------------------------*
       352100-99-FIM.                  EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       352200-VALIDAR-MIDIA            SECTION.
      *----------------------------------------------------------------*

           IF  (GFCT00-EVN-TARIFA-MIDIA OF GFCTMVTA
                                        NOT NUMERIC)
                MOVE 'S'               TO  WRK-IND-ERRO-DETALHE
                MOVE  1140             TO  WRK-COD-MENSAGEM
           END-IF.

      *    MOVE GFCT00-EVN-TARIFA-MIDIA      OF GFCTMVTA
      *                                      TO CTPO-BONIF OF GFCTB0G9.

      *    EXEC SQL
      *        SELECT CTPO_BONIF
      *        INTO   GFCTB0G9.CTPO-BONIF
      *        FROM   DB2PRD.TTPO_MIDIA_SERVC
      *        WHERE  CTPO_BONIF = :GFCTB0G9.CTPO-BONIF    AND
      *        CINDCD_EXCL_LOGCA = :GFCTB0G9.CINDCD-EXCL-LOGCA
      *    END-EXEC.

      *    IF (SQLCODE                 NOT EQUAL ZEROS AND +100) OR
      *       (SQLWARN0                EQUAL 'W'      )
      *        MOVE  8                 TO RETURN-CODE
      *        MOVE 'DB2'              TO ERR-TIPO-ACESSO
      *        MOVE 'TTPO_MIDIA_SERVC' TO ERR-DBD-TAB
      *        MOVE 'SELECT  '         TO ERR-FUN-COMANDO
      *        MOVE  SQLCODE           TO ERR-SQL-CODE
      *        MOVE '0020'             TO ERR-LOCAL
      *        MOVE  SPACES            TO ERR-SEGM
      *        PERFORM 999999-PROCESSAR-ROTINA-ERRO
      *    END-IF.

      *    IF  SQLCODE                 EQUAL +100
      *        MOVE 'S'                TO WRK-IND-ERRO-DETALHE
      *        MOVE  XXXX              TO WRK-COD-MENSAGEM
      *        PERFORM 340000-FORMATAR-TAB-GP
      *    END-IF.

      *----------------------------------------------------------------*
       352200-99-FIM.                  EXIT.
      *----------------------------------------------------------------*
           EJECT
      *----------------------------------------------------------------*
       352300-VALIDAR-PERIFERICO       SECTION.
      *----------------------------------------------------------------*

           MOVE GFCT00-EVN-TARIFA-PERIF      OF GFCTMVTA
                                             TO CTPO-PERIF OF SBINB026.

           EXEC SQL
                SELECT CTPO_PERIF
                INTO   :SBINB026.CTPO-PERIF
                FROM   DB2PRD.PERIF_SBIN
                WHERE  CTPO_PERIF  = :SBINB026.CTPO-PERIF
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS AND +100) OR
              (SQLWARN0                EQUAL 'W'      )
               MOVE  8                 TO RETURN-CODE
               MOVE 'DB2'              TO ERR-TIPO-ACESSO
               MOVE 'PERIF_SBIN'       TO ERR-DBD-TAB
               MOVE 'SELECT  '         TO ERR-FUN-COMANDO
               MOVE  SQLCODE           TO ERR-SQL-CODE
               MOVE '0030'             TO ERR-LOCAL
               MOVE  SPACES            TO ERR-SEGM
               PERFORM 999999-PROCESSAR-ROTINA-ERRO
           END-IF.

           IF  SQLCODE                 EQUAL +100
               MOVE 'S'                TO WRK-IND-ERRO-DETALHE
               MOVE  1141              TO WRK-COD-MENSAGEM
           END-IF.

      *----------------------------------------------------------------*
       352300-99-FIM.                  EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       352350-OBTER-ASSOCIA-MID-PER    SECTION.
      *----------------------------------------------------------------*

           MOVE GFCT00-EVN-TARIFA-MIDIA  OF GFCTMVTA
                                    TO   CTPO-MIDIA        OF GFCTB0G8.

           MOVE GFCT00-EVN-TARIFA-PERIF  OF GFCTMVTA
                                    TO   CTPO-PERIF        OF GFCTB0G8.

           MOVE GFCT00-EVN-DATA-EVENTO   OF GFCTMVTA
                                    TO   DINIC-VGCIA-PERIF OF GFCTB0G8
                                         DFIM-VGCIA-PERIF  OF GFCTB0G8.

           EXEC SQL
                SELECT CTPO_MIDIA ,
                       CTPO_PERIF
                INTO  :GFCTB0G8.CTPO-MIDIA ,
                      :GFCTB0G8.CTPO-PERIF
                FROM   DB2PRD.TCOMPS_MIDIA_PERIF
                WHERE  CTPO_MIDIA         = :GFCTB0G8.CTPO-MIDIA
                AND    CTPO_PERIF         = :GFCTB0G8.CTPO-PERIF
                AND    DINIC_VGCIA_PERIF <= :GFCTB0G8.DINIC-VGCIA-PERIF
                AND    DFIM_VGCIA_PERIF  >= :GFCTB0G8.DFIM-VGCIA-PERIF
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS AND +100) OR
              (SQLWARN0                EQUAL 'W'      )
               MOVE  8                 TO RETURN-CODE
               MOVE 'DB2'              TO ERR-TIPO-ACESSO
               MOVE 'TCOMPS_MIDIA_PERIF'  TO ERR-DBD-TAB
               MOVE 'SELECT  '         TO ERR-FUN-COMANDO
               MOVE  SQLCODE           TO ERR-SQL-CODE
               MOVE '0040'             TO ERR-LOCAL
               MOVE  SPACES            TO ERR-SEGM
               PERFORM 999999-PROCESSAR-ROTINA-ERRO
           END-IF.

      *----------------------------------------------------------------*
       352350-99-FIM.                  EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       352400-VALIDAR-BANCO            SECTION.
      *----------------------------------------------------------------*

           MOVE 'VSR001'               TO P0536-VERSAO.
           MOVE GFCT00-EVN-BCO-ORIGEM  OF GFCTMVTA
                                       TO P0536-COD-BANCO.

           CALL 'POOL0536'             USING P0536-VERSAO
                                             P0536-MENSAGEM
                                             P0536-COD-BANCO
                                             P0536-DIG-BANCO
                                             P0536-NOME-BANCO
                                             P0536-CNPJ
                                             P0536-SIGLA-BANCO
                                             P0536-NOME-USU-BANCO
                                             P0536-DOC-ELETR
                                             P0536-END-BANCO
                                             P0536-CEP-BANCO
                                             P0536-CCEP-BANCO
                                             P0536-COD-MUNIC
                                             P0536-NOME-MUNIC
                                             P0536-DT-INCL
                                             P0536-SITU-BANCO.

           IF  RETURN-CODE             NOT EQUAL ZEROS AND 8
               MOVE 8                  TO RETURN-CODE
               MOVE WRK-MSG-0536       TO ERR-TEXTO
               PERFORM 999999-PROCESSAR-ROTINA-ERRO
           ELSE
               IF  RETURN-CODE         EQUAL 8
                   MOVE 'S'            TO WRK-IND-ERRO-DETALHE
                   MOVE  1093          TO WRK-COD-MENSAGEM
               END-IF
           END-IF.

      *----------------------------------------------------------------*
       352400-99-FIM.                  EXIT.
      *----------------------------------------------------------------*
           EJECT
      *----------------------------------------------------------------*
       352500-VALIDAR-AGENCIA          SECTION.
      *----------------------------------------------------------------*

           IF ( GFCT00-EVN-BCO-ORIGEM   OF GFCTMVTA NOT NUMERIC   OR
                GFCT00-EVN-BCO-ORIGEM   OF GFCTMVTA EQUAL ZEROS ) OR
              ( GFCT00-EVN-AGE-ORIGEM   OF GFCTMVTA NOT NUMERIC   OR
                GFCT00-EVN-AGE-ORIGEM   OF GFCTMVTA EQUAL ZEROS )
                GO TO 352500-99-FIM.

           MOVE GFCT00-EVN-BCO-ORIGEM  OF GFCTMVTA
                                       TO COMU-BANCO.
           MOVE GFCT00-EVN-AGE-ORIGEM  OF GFCTMVTA
                                       TO COMU-DEPDC.
           MOVE 'GFCT1105'             TO COMU-NOME-PGM.
           MOVE 'MESU9015'             TO WRK-MODULO.

           CALL WRK-MODULO             USING COMU-PARM
                                             WRK-POOL7100
                                             WRK-SQLCA.

           IF  COMU-COD-RETORNO        NOT EQUAL ZEROS
               IF  COMU-COD-RETORNO    EQUAL 1
                   MOVE 'S'            TO WRK-IND-ERRO-DETALHE
                   MOVE  1081          TO WRK-COD-MENSAGEM
               ELSE
                   MOVE 8              TO RETURN-CODE
                   MOVE COMU-COD-RETORNO
                                       TO WRK-COD-RET-9015
                   MOVE WRK-MSG-9015   TO ERR-TEXTO
                   PERFORM 999999-PROCESSAR-ROTINA-ERRO
               END-IF
           END-IF.

      *----------------------------------------------------------------*
       352500-99-FIM.                  EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       352600-VALIDAR-PAB-ORIGEM       SECTION.
      *----------------------------------------------------------------*

           MOVE 'VRS001'               TO COMU-VERSAO.
           MOVE GFCT00-EVN-BCO-ORIGEM  OF GFCTMVTA
                                       TO COMU-COD-EMPR.
           MOVE GFCT00-EVN-AGE-ORIGEM  OF GFCTMVTA
                                       TO COMU-COD-DEPDC.
           MOVE GFCT00-EVN-PAB-ORIGEM  OF GFCTMVTA
                                       TO COMU-COD-POSTO.
           MOVE SPACES                 TO COMU-CORR-BANCARIO
           MOVE 'MESU9018'             TO WRK-MODULO.

           CALL WRK-MODULO             USING COMU-DADOS-9018.

      *--- MSG001 '001-OPERACAO EFETUADA COM SUCESSO'.
      *--- MSG002 '002-VERSAO INVALIDA'.
      *--- MSG003 '003-TAB PESQUISADA INDISPON NO MOMENTO'.
      *--- MSG006 '006-COD DE EMPRESA INCONSISTENTE'.
      *--- MSG008 '008-COD DE DEPEND. INCONSISTENTE'.
      *--- MSG014 '014-COD DE POSTO INCONSISTENTE'.
      *--- MSG025 '025-ERRO DB2'.
      *--- MSG007 '007-COD EMPRESA NAO ENCONTRADO'.
      *--- MSG009 '009-COD DEPENDENCIA NAO ENCONTRADO'.
      *--- MSG015 '015-COD POSTO NAO ENCONTRADO'.
      *--- MSG019 '019-COD CORRESP BANCARIO NAO ENCONTRADO'.

           IF  COMU-TEXTO-MSG(1:3)     NOT EQUAL '001'
               IF  COMU-TEXTO-MSG(1:3) EQUAL     '002' OR '003' OR
                                                 '006' OR '008' OR
                                                 '014' OR '025'
                   MOVE 8              TO RETURN-CODE
                   MOVE WRK-MSG-9018   TO ERR-TEXTO
                   PERFORM 999999-PROCESSAR-ROTINA-ERRO
               ELSE
      *************MOVE 'S'            TO WRK-IND-ERRO-DETALHE
      *************MOVE  1082          TO WRK-COD-MENSAGEM
                   MOVE  ZEROS         TO GFCT00-EVN-PAB-ORIGEM
                                          OF GFCTMVTA
               END-IF.

      *----------------------------------------------------------------*
       352600-99-FIM.                  EXIT.
      *----------------------------------------------------------------*
           EJECT
      *----------------------------------------------------------------*
       352650-VALIDAR-PAB-DEB-CRED     SECTION.
      *----------------------------------------------------------------*

           MOVE 'VRS001'                  TO COMU-VERSAO.
           MOVE  GFCT00-EVN-BCO-DEB-CRED  OF GFCTMVTA
                                          TO COMU-COD-EMPR.
           MOVE  GFCT00-EVN-AGE-DEB-CRED  OF GFCTMVTA
                                          TO COMU-COD-DEPDC.
           MOVE WRK-POSTO-SERVC           TO COMU-COD-POSTO.
           MOVE SPACES                    TO COMU-CORR-BANCARIO.
           MOVE 'MESU9018'                TO WRK-MODULO.

           CALL WRK-MODULO      USING     COMU-DADOS-9018.

           IF  COMU-TEXTO-MSG(1:3)     NOT EQUAL '001'
               IF  COMU-TEXTO-MSG(1:3) EQUAL     '002' OR '003' OR
                                                 '006' OR '008' OR
                                                 '014' OR '025'
                   MOVE 8              TO RETURN-CODE
                   MOVE WRK-MSG-9018   TO ERR-TEXTO
                   PERFORM 999999-PROCESSAR-ROTINA-ERRO
               ELSE
                   MOVE 'S'            TO WRK-IND-ERRO-DETALHE
                   MOVE  1082          TO WRK-COD-MENSAGEM
               END-IF.

      *----------------------------------------------------------------*
       352650-99-FIM.                  EXIT.
      *----------------------------------------------------------------*
           EJECT
      *----------------------------------------------------------------*
       352700-VALIDAR-AGE-CONTA-CLIE   SECTION.
      *----------------------------------------------------------------*

FAIRES     IF   LNK-MODULO                  EQUAL  'PERF'
  "         IF  WRK-INDIC-CLIEV008          EQUAL  'S'
  "             MOVE ZEROS                  TO  SQLCODE
  "             MOVE WRK-POSTO-SERVC-E      TO  CPOSTO-SERVC OF CLIEV008
  "             MOVE WRK-CID-CLI-E          TO  CID-CLI      OF CLIEV008
  "             MOVE WRK-CGRP-CTBIL-ATUAL-E TO  CGRP-CTBIL-ATUAL
  "                                         OF  CLIEV008
  "             MOVE WRK-CSGRP-CTBIL-E      TO  CSGRP-CTBIL  OF CLIEV008
  "         ELSE
  "             MOVE +100                   TO  SQLCODE
  "         END-IF
  "             GO     TO   352700-50-TRATAR-SQLCODE
FAIRES     END-IF.

           MOVE GFCT00-EVN-AGE-DEB-CRED OF GFCTMVTA
                                        TO CJUNC-DEPDC OF CLIEV008.
           MOVE GFCT00-EVN-CTA-DEB-CRED OF GFCTMVTA
                                        TO CCTA-CLI    OF CLIEV008.

           EXEC SQL
             SELECT
                   CJUNC_DEPDC,
                   CCTA_CLI,
                   CGRP_CTBIL_ATUAL,
                   CSGRP_CTBIL,
                   CPOSTO_SERVC,
                   CID_CLI
             INTO
                   :CLIEV008.CJUNC-DEPDC,
                   :CLIEV008.CCTA-CLI,
                   :CLIEV008.CGRP-CTBIL-ATUAL,
                   :CLIEV008.CSGRP-CTBIL,
                   :CLIEV008.CPOSTO-SERVC,
                   :CLIEV008.CID-CLI
             FROM   DB2PRD.V01CTA_CORRENTE
             WHERE
                   CJUNC_DEPDC        = :CLIEV008.CJUNC-DEPDC        AND
                   CCTA_CLI           = :CLIEV008.CCTA-CLI
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS AND +100) OR
              (SQLWARN0                EQUAL 'W'      )
               MOVE 8                  TO RETURN-CODE
               MOVE 'DB2'              TO ERR-TIPO-ACESSO
               MOVE 'V01CTA_CORRENTE ' TO ERR-DBD-TAB
               MOVE 'SELECT  '         TO ERR-FUN-COMANDO
               MOVE  SQLCODE           TO ERR-SQL-CODE
               MOVE  '0050'            TO ERR-LOCAL
               MOVE  SPACES            TO ERR-SEGM
               PERFORM 999999-PROCESSAR-ROTINA-ERRO
           END-IF.

FAIRES*
  "    352700-50-TRATAR-SQLCODE.
FAIRES*--------------------------*


           IF  SQLCODE                 EQUAL +100
FAIRES         ADD   1                 TO WRK-ACU-NACHOU-CLIEV008
               MOVE 'S'                TO WRK-IND-ERRO-DETALHE-CLIE
                                          GFCT00-HDR-ERRO-LOGICO
                                                     OF GFCTMVTA
               MOVE  1083              TO WRK-COD-MENSAGEM
               PERFORM 340000-FORMATAR-TAB-GP
           ELSE
FAIRES         ADD   1                 TO WRK-ACU-ACHOU-CLIEV008
               MOVE  CPOSTO-SERVC      OF CLIEV008
                                       TO WRK-POSTO-SERVC
               MOVE  CID-CLI           OF CLIEV008
                                       TO WRK-CID-CLI
               MOVE  CGRP-CTBIL-ATUAL  OF CLIEV008
                                       TO WRK-CGRP-CTBIL-ATUAL
               MOVE  CSGRP-CTBIL       OF CLIEV008
                                       TO WRK-CSGRP-CTBIL
               IF WRK-CGRP-CTBIL-ATUAL EQUAL 18 AND
                  WRK-CSGRP-CTBIL      EQUAL 79
                  MOVE 'I' TO WRK-TIPO-CONTA
               ELSE
CI0815            IF   GFCT00-EVN-TIPO-CONTA OF GFCTMVTA EQUAL 2
CI0815                 MOVE 'F' TO WRK-TIPO-CONTA
CI0815            ELSE
                       MOVE 'C' TO WRK-TIPO-CONTA
CI0815            END-IF
               END-IF
           END-IF.

      *----------------------------------------------------------------*
       352700-99-FIM.                  EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       352750-VALIDAR-AGE-CTA-CLIE-P    SECTION.
      *----------------------------------------------------------------*

FAIRES     IF   LNK-MODULO                EQUAL  'PERF'
  "         IF  WRK-INDIC-CLIEV007        EQUAL  'S'
  "             MOVE   ZEROS                TO  SQLCODE
  "             MOVE   WRK-POSTO-SERVC-E    TO  CPOSTO-SERVC OF CLIEV007
  "             MOVE   WRK-CID-CLI-E        TO  CID-CLI      OF CLIEV007
  "         ELSE
  "             MOVE   +100                 TO  SQLCODE
  "         END-IF
  "             GO     TO   352750-50-TRATAR-SQLCODE
FAIRES     END-IF.

           MOVE GFCT00-EVN-AGE-DEB-CRED OF GFCTMVTA
                                        TO CJUNC-DEPDC OF CLIEV007.
           MOVE GFCT00-EVN-CTA-DEB-CRED OF GFCTMVTA
                                        TO CCTA-CLI    OF CLIEV007.

           EXEC SQL
             SELECT
                   CJUNC_DEPDC,
                   CCTA_CLI,
                   CPOSTO_SERVC,
                   CID_CLI
             INTO
                   :CLIEV007.CJUNC-DEPDC,
                   :CLIEV007.CCTA-CLI,
                   :CLIEV007.CPOSTO-SERVC,
                   :CLIEV007.CID-CLI
             FROM   DB2PRD.V01CTA_POUPANCA
             WHERE
                   CJUNC_DEPDC        = :CLIEV007.CJUNC-DEPDC        AND
                   CCTA_CLI           = :CLIEV007.CCTA-CLI
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS AND +100) OR
              (SQLWARN0                EQUAL 'W'      )
               MOVE 8                  TO RETURN-CODE
               MOVE 'DB2'              TO ERR-TIPO-ACESSO
               MOVE 'V01CTA_POUPANCA ' TO ERR-DBD-TAB
               MOVE 'SELECT  '         TO ERR-FUN-COMANDO
               MOVE  SQLCODE           TO ERR-SQL-CODE
               MOVE  '0060'            TO ERR-LOCAL
               MOVE  SPACES            TO ERR-SEGM
               PERFORM 999999-PROCESSAR-ROTINA-ERRO
           END-IF.

FAIRES*
  "    352750-50-TRATAR-SQLCODE.
FAIRES*--------------------------*


           IF  SQLCODE                 EQUAL +100
FAIRES         ADD   1                 TO WRK-ACU-NACHOU-CLIEV007
               MOVE 'S'                TO WRK-IND-ERRO-DETALHE-CLIE
                                          GFCT00-HDR-ERRO-LOGICO
                                                   OF   GFCTMVTA
               MOVE  1083              TO WRK-COD-MENSAGEM
               PERFORM 340000-FORMATAR-TAB-GP
           ELSE
FAIRES         ADD   1                 TO WRK-ACU-ACHOU-CLIEV007
               MOVE  CPOSTO-SERVC      OF CLIEV007
                                       TO WRK-POSTO-SERVC
               MOVE  CID-CLI           OF CLIEV007
                                       TO WRK-CID-CLI
               MOVE 'P'                TO WRK-TIPO-CONTA
           END-IF.

      *----------------------------------------------------------------*
       352750-99-FIM.                  EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       352760-VALIDAR-TIPO-PESSOA       SECTION.
      *----------------------------------------------------------------*

FAIRES     IF   LNK-MODULO                EQUAL  'PERF'
  "         IF  WRK-INDIC-CLIEV004        EQUAL  'S'
  "             MOVE   ZEROS                TO  SQLCODE
  "             MOVE   WRK-TIPO-PSSOA-E     TO  CTPO-PSSOA   OF CLIEV004
  "         ELSE
  "             MOVE   +100                 TO  SQLCODE
  "         END-IF
  "             GO     TO   352760-50-TRATAR-SQLCODE
FAIRES     END-IF.


           MOVE WRK-CID-CLI             TO CID-CLI     OF CLIEV004.

           EXEC SQL
             SELECT
                   CTPO_PSSOA
             INTO
                   :CLIEV004.CTPO-PSSOA
             FROM   DB2PRD.V01CLIENTE_AGENCIA
             WHERE
                   CID_CLI = :CLIEV004.CID-CLI
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS AND +100) OR
              (SQLWARN0                EQUAL 'W'      )
               MOVE 8                  TO RETURN-CODE
               MOVE 'DB2'              TO ERR-TIPO-ACESSO
               MOVE 'V01CLIENTE_AGENCIA ' TO ERR-DBD-TAB
               MOVE 'SELECT  '         TO ERR-FUN-COMANDO
               MOVE  SQLCODE           TO ERR-SQL-CODE
               MOVE  '0200'            TO ERR-LOCAL
               MOVE  SPACES            TO ERR-SEGM
               PERFORM 999999-PROCESSAR-ROTINA-ERRO
           END-IF.

FAIRES*
  "    352760-50-TRATAR-SQLCODE.
FAIRES*--------------------------*


           IF  SQLCODE                 EQUAL +100
FAIRES         ADD   1                 TO WRK-ACU-NACHOU-CLIEV004
               MOVE 'S'                TO WRK-IND-ERRO-DETALHE
               MOVE  1304              TO WRK-COD-MENSAGEM
           ELSE
FAIRES         ADD   1                 TO WRK-ACU-ACHOU-CLIEV004
               IF    CTPO-PSSOA  OF CLIEV004  EQUAL 'F'
                     MOVE  '1'   TO WRK-TIPO-PSSOA-CLIE
               ELSE
                     MOVE  '2'   TO WRK-TIPO-PSSOA-CLIE
               END-IF
               IF  ( WRK-TIPO-PSSOA-CLIE
                                 NOT EQUAL WRK-5318-CPSSOA-SERVC-TARIF )
                                       AND
                   ( WRK-5318-CPSSOA-SERVC-TARIF NOT EQUAL 3 )
                     MOVE 'S'    TO WRK-IND-ERRO-DETALHE
                     MOVE  1305  TO WRK-COD-MENSAGEM
               END-IF
           END-IF.

      *----------------------------------------------------------------*
       352760-99-FIM.                  EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       352800-VALIDAR-MOEDA            SECTION.
      *----------------------------------------------------------------*

           IF   GFCT00-EVN-DATA-CONV   OF GFCTMVTA
                                       EQUAL           SPACES
                   MOVE 'S'            TO WRK-IND-ERRO-DETALHE
                   MOVE  1122          TO WRK-COD-MENSAGEM
                   GO TO 352800-99-FIM
           END-IF

           MOVE 237                    TO NTAX-CODIGO-BANCO.
           MOVE GFCT00-EVN-TIPO-MOEDA  OF GFCTMVTA
                                       TO NTAX-CODIGO-TAXA.
           MOVE GFCT00-EVN-DATA-CONV   OF GFCTMVTA
                                       TO WRK-DATA-AUX.
           MOVE WRK-DIA-AUX            TO WRK-DIA-AUX2.
           MOVE WRK-MES-AUX            TO WRK-MES-AUX2.
           MOVE WRK-ANO-AUX            TO WRK-ANO-AUX2.
           MOVE WRK-DATA-AUX2          TO NTAX-DATA-INICIO.
           MOVE GFCT00-EVN-DATA-CONV   OF GFCTMVTA
                                       TO WRK-DATA-AUX.
           MOVE WRK-DIA-AUX            TO WRK-DIA-AUX2.
           MOVE WRK-MES-AUX            TO WRK-MES-AUX2.
           MOVE WRK-ANO-AUX            TO WRK-ANO-AUX2.
           MOVE WRK-DATA-AUX2          TO NTAX-DATA-FIM.
           MOVE 'NTAX5000'             TO WRK-MODULO.

           CALL WRK-MODULO             USING NTAX-PESQUISA-TAXA.

           IF  NTAX-CODIGO-RETORNO     NOT EQUAL ZEROS
               IF  NTAX-CODIGO-RETORNO EQUAL 2 OR 7
                   MOVE 'S'            TO WRK-IND-ERRO-DETALHE
                   MOVE  1084          TO WRK-COD-MENSAGEM
               ELSE
                   MOVE 8              TO RETURN-CODE
                   MOVE NTAX-CODIGO-RETORNO
                                       TO WRK-COD-RET-NTAX
                   MOVE WRK-MSG-NTAX   TO ERR-TEXTO
                   PERFORM 999999-PROCESSAR-ROTINA-ERRO
               END-IF
           END-IF.

      *----------------------------------------------------------------*
       352800-99-FIM.                  EXIT.
      *----------------------------------------------------------------*
           EJECT

      *----------------------------------------------------------------*
       352850-VALIDAR-MOEDA-TARIFA     SECTION.
      *----------------------------------------------------------------*

           MOVE 237                    TO NTAX-CODIGO-BANCO.
           MOVE WRK-5318-CMOEDA-BACEN(IND-2)
                                       TO NTAX-CODIGO-TAXA.
           MOVE GFCT00-EVN-DATA-EVENTO OF GFCTMVTA
                                       TO WRK-DATA-AUX.
           MOVE WRK-DIA-AUX            TO WRK-DIA-AUX2.
           MOVE WRK-MES-AUX            TO WRK-MES-AUX2.
           MOVE WRK-ANO-AUX            TO WRK-ANO-AUX2.
           MOVE WRK-DATA-AUX2          TO NTAX-DATA-INICIO.
           MOVE GFCT00-EVN-DATA-EVENTO OF GFCTMVTA
                                       TO WRK-DATA-AUX.
           MOVE WRK-DIA-AUX            TO WRK-DIA-AUX2.
           MOVE WRK-MES-AUX            TO WRK-MES-AUX2.
           MOVE WRK-ANO-AUX            TO WRK-ANO-AUX2.
           MOVE WRK-DATA-AUX2          TO NTAX-DATA-FIM.
           MOVE 'NTAX5000'             TO WRK-MODULO.

           CALL WRK-MODULO             USING NTAX-PESQUISA-TAXA.

           MOVE NTAX-CODIGO-RETORNO    TO    WRK-COD-RET-NTAX.

           IF NTAX-CODIGO-RETORNO     NOT EQUAL ZEROS AND 2 AND 7
                                                      AND 4 AND 5
              MOVE  8                 TO      RETURN-CODE
              MOVE  NTAX-CODIGO-RETORNO
                                      TO      WRK-COD-RET-NTAX
              MOVE  WRK-MSG-NTAX      TO      ERR-TEXTO
              PERFORM 999999-PROCESSAR-ROTINA-ERRO
           END-IF.

      *----------------------------------------------------------------*
       352850-99-FIM.                  EXIT.
      *----------------------------------------------------------------*
           EJECT

      *----------------------------------------------------------------*
       352900-CHECA-VALOR-TARIFA       SECTION.
      *----------------------------------------------------------------*

           MOVE ZEROS                  TO   IND-2
           MOVE ' '                    TO   WRK-TEM-PERCENTUAL
           MOVE ' '                    TO   WRK-IND-TAR-MOEDA-EVENTO
           MOVE ZEROS                  TO   WRK-COD-RET-NTAX.

           PERFORM 352930-VARRE        VARYING
                   IND-2   FROM 1 BY 1 UNTIL
                   IND-2               GREATER 50 OR
                                       WRK-5318-DINIC-VGCIA-TARIF
                                    OF WRK-5318-DADOS-DE-VALORES(IND-2)
                                       EQUAL SPACES.

      *----------------------------------------------------------------*
       352900-99-FIM.                  EXIT.
      *----------------------------------------------------------------*
           EJECT

      *----------------------------------------------------------------*
       352910-OBTER-DADOS-TARIFA       SECTION.
      *----------------------------------------------------------------*

           MOVE 'B'                        TO    WRK-5318-AMBIENTE.
           IF GFCT00-EVN-CD-TARI-CORR-R  OF GFCTMVTA EQUAL ZEROS
              MOVE GFCT00-EVN-COD-TARIFA  OF GFCTMVTA TO
                   WRK-CAMPO-TIRA-SINAL-17
              MOVE '+' TO WRK-CAMPO-TIRA-SINAL-S
              MOVE WRK-CAMPO-TIRA-SINAL-05 TO WRK-5318-CODIGO-TARIFA
           ELSE
              MOVE GFCT00-EVN-CD-TARI-CORR-R  OF GFCTMVTA TO
                   WRK-CAMPO-TIRA-SINAL-17
              MOVE '+' TO WRK-CAMPO-TIRA-SINAL-S
              MOVE WRK-CAMPO-TIRA-SINAL-05 TO WRK-5318-CODIGO-TARIFA
           END-IF.


           MOVE  GFCT00-EVN-DATA-EVENTO OF GFCTMVTA
                                           TO    WRK-5318-DT-REFER.


FAIRES     IF ((WRK-5318-DT-REFER      NOT EQUAL WRK-DT-INI-VIGEN-ANT2)
                                          OR
  "            (WRK-5318-CODIGO-TARIFA NOT EQUAL WRK-CODIGO-TARIF-ANT2))
  "             MOVE WRK-5318-DT-REFER        TO WRK-DT-INI-VIGEN-ANT2
  "             MOVE WRK-5318-CODIGO-TARIFA   TO WRK-CODIGO-TARIF-ANT2
  "             MOVE 'GFCT5318'               TO WRK-MODULO
  "
  "             CALL WRK-MODULO        USING     WRK-5318-AREA-ENTRADA
  "                                              WRK-5318-AREA-SAIDA
  "                                              WRK-5318-DADOS-RETORNO
  "             IF  WRK-5318-COD-RETORNO NOT EQUAL ZEROS
  "                 MOVE 08                     TO RETURN-CODE
  "                 MOVE WRK-5318-COD-RETORNO   TO WRK-COD-RET-X
  "                 MOVE WRK-ERRO-GFCT5318      TO ERR-TEXTO
  "                 PERFORM 999999-PROCESSAR-ROTINA-ERRO
  "             END-IF
FAIRES      END-IF.

      *----------------------------------------------------------------*
       352910-99-FIM.                  EXIT.
      *----------------------------------------------------------------*
           EJECT

      *----------------------------------------------------------------*
       352930-VARRE                    SECTION.
      *----------------------------------------------------------------*

           IF  WRK-5318-PSERVC-PTADO(IND-2)  NOT EQUAL    0
               MOVE  'S'                     TO  WRK-TEM-PERCENTUAL
           END-IF.

           IF (WRK-5318-VCOMPO-TARIF(IND-2)  NOT EQUAL    0) AND
              (WRK-5318-CMOEDA-BACEN(IND-2)  NOT EQUAL    0) AND
              (WRK-5318-CMOMEN-CONVS-MOEDA(IND-2)
                                             EQUAL        1)
               MOVE  'S'                     TO WRK-IND-TAR-MOEDA-EVENTO
               PERFORM 352850-VALIDAR-MOEDA-TARIFA
           END-IF.

      *----------------------------------------------------------------*
       352930-99-FIM.                  EXIT.
      *----------------------------------------------------------------*
           EJECT

      *----------------------------------------------------------------*
       352990-ACESSA-DPROCM            SECTION.
      *----------------------------------------------------------------*

           MOVE 'GFCT'                 TO  CSIST-PRINC
                                       OF  GFCTB0A1.

           EXEC SQL
                SELECT CSIST_PRINC  ,
                       DPROCM_ANTER ,
                       DPROCM_ATUAL ,
                       DPROCM_PROX
                INTO  :GFCTB0A1.CSIST-PRINC  ,
                      :GFCTB0A1.DPROCM-ANTER ,
                      :GFCTB0A1.DPROCM-ATUAL ,
                      :GFCTB0A1.DPROCM-PROX
                FROM   DB2PRD.PARM_DATA_PROCM
                WHERE  CSIST_PRINC = :GFCTB0A1.CSIST-PRINC
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS) OR
              (SQLWARN0                EQUAL 'W'      )
               MOVE 8                  TO RETURN-CODE
               MOVE 'DB2'              TO ERR-TIPO-ACESSO
               MOVE 'PARM_DATA_PROCM ' TO ERR-DBD-TAB
               MOVE 'SELECT  '         TO ERR-FUN-COMANDO
               MOVE  SQLCODE           TO ERR-SQL-CODE
               MOVE  '0070'            TO ERR-LOCAL
               MOVE  SPACES            TO ERR-SEGM
               PERFORM 999999-PROCESSAR-ROTINA-ERRO
           END-IF.

           MOVE DPROCM-ATUAL              OF     GFCTB0A1
                                          TO     WRK-DATA-AUX.
           MOVE WRK-ANO-AUX               TO     WRK-ANO-ATU
           MOVE WRK-MES-AUX               TO     WRK-MES-ATU
           MOVE WRK-DIA-AUX               TO     WRK-DIA-ATU.

           MOVE DPROCM-PROX               OF     GFCTB0A1
                                          TO     WRK-DATA-AUX.
           MOVE WRK-ANO-AUX               TO     WRK-ANO-PROX
           MOVE WRK-MES-AUX               TO     WRK-MES-PROX
           MOVE WRK-DIA-AUX               TO     WRK-DIA-PROX.

      *----------------------------------------------------------------*
       352990-99-FIM.                  EXIT.
      *----------------------------------------------------------------*
           EJECT

      *----------------------------------------------------------------*
       353000-CONSISTIR-DET-DESCONTO   SECTION.
      *----------------------------------------------------------------*

           IF  GFCT00-DSC-TIPO-DESC    OF GFCTMVTA
                                       EQUAL 1
      *******  PERFORM 353100-VALIDAR-COD-DESC-BONIF
               IF  WRK-IND-ERRO-DETALHE EQUAL 'S'
                   MOVE 'S'            TO WRK-INDREG-REJ
                                          WRK-IND-ERRO-GRUPO
                                          GFCT00-HDR-ERRO-LOGICO
                                                           OF GFCTMVTA
                   PERFORM 340000-FORMATAR-TAB-GP
                   MOVE SPACES         TO WRK-IND-ERRO-DETALHE
               END-IF
           END-IF.

           IF  GFCT00-DSC-TIPO-DESC    OF GFCTMVTA
                                       EQUAL 2
      ******** PERFORM 353200-VALIDAR-COD-DESC-FLEX
               IF  WRK-IND-ERRO-DETALHE EQUAL 'S'
                   MOVE 'S'            TO WRK-INDREG-REJ
                                          WRK-IND-ERRO-GRUPO
                                          GFCT00-HDR-ERRO-LOGICO
                                                           OF GFCTMVTA
                   PERFORM 340000-FORMATAR-TAB-GP
                   MOVE SPACES         TO WRK-IND-ERRO-DETALHE
               END-IF
           END-IF.

           IF  GFCT00-DSC-TIPO-DESC    OF GFCTMVTA
                                       EQUAL 2
      *******  PERFORM 353300-VALIDAR-CONTRATO
               IF  WRK-IND-ERRO-DETALHE EQUAL 'S'
                   MOVE 'S'            TO WRK-INDREG-REJ
                                          WRK-IND-ERRO-GRUPO
                                          GFCT00-HDR-ERRO-LOGICO
                                                           OF GFCTMVTA
                   PERFORM 340000-FORMATAR-TAB-GP
                   MOVE SPACES         TO WRK-IND-ERRO-DETALHE
               END-IF
           END-IF.

           IF  GFCT00-DSC-GRUPO-CONTA  OF GFCTMVTA
                                       NOT EQUAL ZEROS
               PERFORM 353400-VALIDAR-GP-CONTA
               IF  WRK-IND-ERRO-DETALHE EQUAL 'S'
                   MOVE 'S'            TO WRK-INDREG-REJ
                                          WRK-IND-ERRO-GRUPO
                                          GFCT00-HDR-ERRO-LOGICO
                                                           OF GFCTMVTA
                   PERFORM 340000-FORMATAR-TAB-GP
                   MOVE SPACES         TO WRK-IND-ERRO-DETALHE
               END-IF
           END-IF.


      *----------------------------------------------------------------*
       353000-99-FIM.                  EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       3540-CONSISTIR-DET-COMP         SECTION.
      *----------------------------------------------------------------*

           IF GFCT00-COM-USERID OF GFCTMVTA IS NUMERIC AND
              GFCT00-COM-USERID OF GFCTMVTA NOT EQUAL ZEROS
              PERFORM 3541-VALIDAR-COD-USUARIO

              IF  WRK-IND-ERRO-DETALHE EQUAL 'S'
                  MOVE 'S'            TO WRK-INDREG-REJ
                                         WRK-IND-ERRO-GRUPO
                                         GFCT00-HDR-ERRO-LOGICO
                                         OF GFCTMVTA
                  PERFORM 340000-FORMATAR-TAB-GP
                  MOVE SPACES         TO WRK-IND-ERRO-DETALHE
              END-IF
           END-IF.

      *----------------------------------------------------------------*
       3540-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *    ROTINA PARA VALIDAR O CODIGO DO FUNCIONARIO                 *
      ******************************************************************
      *----------------------------------------------------------------*
       3541-VALIDAR-COD-USUARIO        SECTION.
      *----------------------------------------------------------------*

           MOVE ZEROS                         TO WRK-COD-FUNC2.

           MOVE GFCT00-COM-USERID OF GFCTMVTA TO WRK-COD-FUNC.

           MOVE WRK-COD-FUNC           TO WRK-PESQUISA-ENT.
           MOVE 002                    TO WRK-CAMPO-ENT(1).
           MOVE 086                    TO WRK-TAMANHO-ENT.
           MOVE 'SARH8000'             TO WRK-MODULO.

           CALL WRK-MODULO             USING WRK-ENTRADA
                                             WRK-SAIDA
                                             WRK-ERRO-AREA
                                             WRK-SQLCA.

           IF  WRK-COD-RETORNO-SAI     NOT EQUAL ZEROS AND 2
               MOVE 8                  TO RETURN-CODE
               MOVE WRK-COD-RETORNO-SAI
                                       TO WRK-COD-RET-8000
               MOVE WRK-MSG-8000       TO ERR-TEXTO
               PERFORM 999999-PROCESSAR-ROTINA-ERRO
           ELSE
               IF  WRK-COD-RETORNO-SAI EQUAL 2
                   MOVE 'S'            TO WRK-IND-ERRO-DETALHE
                   MOVE  1246          TO WRK-COD-MENSAGEM
               END-IF
           END-IF.

      *----------------------------------------------------------------*
       3541-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *353100-VALIDAR-COD-DESC-BONIF   SECTION.
      *----------------------------------------------------------------*
      *
      *    MOVE GFCT00-DSC-CODIGO-DESC OF GFCTMVTA
      *                                TO CTPO-BONIF       OF GFCTB0F4.
      *
      *    EXEC SQL
      *      SELECT
      *            CTPO_BONIF
      *      INTO
      *            :GFCTB0F4.CTPO-BONIF
      *      FROM   DB2PRD.TPO_BONIF_SERVC
      *      WHERE
      *            CTPO_BONIF          = :GFCTB0F4.CTPO-BONIF
      *    END-EXEC.
      *
      *    IF (SQLCODE                 NOT EQUAL ZEROS AND +100) OR
      *       (SQLWARN0                EQUAL 'W'      )
      *        MOVE 8                  TO RETURN-CODE
      *        MOVE 'DB2'              TO ERR-TIPO-ACESSO
      *        MOVE 'TPO_BONIF_SERVC ' TO ERR-DBD-TAB
      *        MOVE 'SELECT  '         TO ERR-FUN-COMANDO
      *        MOVE  SQLCODE           TO ERR-SQL-CODE
      *        MOVE  '0080'            TO ERR-LOCAL
      *        MOVE  SPACES            TO ERR-SEGM
      *        PERFORM 999999-PROCESSAR-ROTINA-ERRO
      *    END-IF.
      *
      *    IF  SQLCODE                 EQUAL +100
      *        MOVE 'S'                TO WRK-IND-ERRO-DETALHE
      *        MOVE  1085              TO WRK-COD-MENSAGEM
      *    END-IF.
      *
      *----------------------------------------------------------------*
      *353100-99-FIM.                  EXIT.
      *----------------------------------------------------------------*
      *    EJECT
      *----------------------------------------------------------------*
      *353200-VALIDAR-COD-DESC-FLEX    SECTION.
      *----------------------------------------------------------------*
      *
      *    MOVE GFCT00-DSC-CODIGO-DESC OF GFCTMVTA
      *                                TO CAGPTO-CTA       OF GFCTB0B2.
      *
      *    EXEC SQL
      *      SELECT
      *            CAGPTO_CTA,
      *            DINIC_FLEXZ_AGPTO,
      *            DFIM_FLEXZ_AGPTO
      *      INTO
      *            :GFCTB0B2.CAGPTO-CTA,
      *            :GFCTB0B2.DINIC-FLEXZ-AGPTO,
      *            :GFCTB0B2.DFIM-FLEXZ-AGPTO
      *      FROM   DB2PRD.PDIDO_FLEXZ_AGPTO
      *      WHERE
      *            CAGPTO_CTA         = :GFCTB0B2.CAGPTO-CTA
      *    END-EXEC.
      *
      *    IF (SQLCODE                 NOT EQUAL ZEROS AND +100) OR
      *       (SQLWARN0                EQUAL 'W'      )
      *        MOVE 8                  TO RETURN-CODE
      *        MOVE 'DB2'              TO ERR-TIPO-ACESSO
      *        MOVE 'PDIDO_FLEXZ_AGPTO' TO ERR-DBD-TAB
      *        MOVE 'SELECT  '         TO ERR-FUN-COMANDO
      *        MOVE  SQLCODE           TO ERR-SQL-CODE
      *        MOVE  '0090'            TO ERR-LOCAL
      *        MOVE  SPACES            TO ERR-SEGM
      *        PERFORM 999999-PROCESSAR-ROTINA-ERRO
      *    END-IF.
      *
      *    IF  SQLCODE                 EQUAL +100
      *        MOVE 'S'                TO WRK-IND-ERRO-DETALHE
      *        MOVE  1086              TO WRK-COD-MENSAGEM
      *        GO TO 353200-99-FIM
      *    END-IF.
      *
      *    MOVE WRK-DATA-EVN-AUX       TO WRK-DATA-AUX.
      *    MOVE WRK-DIA-AUX            TO WRK-DIA-EVENTO.
      *    MOVE WRK-MES-AUX            TO WRK-MES-EVENTO.
      *    MOVE WRK-ANO-AUX            TO WRK-ANO-EVENTO.
      *
      *    MOVE DINIC-FLEXZ-AGPTO      OF GFCTB0B2
      *                                TO WRK-DATA-AUX.
      *    MOVE WRK-DIA-AUX            TO WRK-DIA-INI.
      *    MOVE WRK-MES-AUX            TO WRK-MES-INI.
      *    MOVE WRK-ANO-AUX            TO WRK-ANO-INI.
      *
      *    MOVE DFIM-FLEXZ-AGPTO       OF GFCTB0B2
      *                                TO WRK-DATA-AUX.
      *    MOVE WRK-DIA-AUX            TO WRK-DIA-FIM.
      *    MOVE WRK-MES-AUX            TO WRK-MES-FIM.
      *    MOVE WRK-ANO-AUX            TO WRK-ANO-FIM.
      *
      *    IF  WRK-DATA-EVENTO         LESS WRK-DATA-INI   OR
      *        WRK-DATA-EVENTO         GREATER WRK-DATA-FIM
      *        MOVE  1087              TO WRK-COD-MENSAGEM
      *        MOVE 'S'                TO WRK-IND-ERRO-DETALHE
      *    END-IF.
      *
      *----------------------------------------------------------------*
      *353200-99-FIM.                  EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *353300-VALIDAR-CONTRATO         SECTION.
      *----------------------------------------------------------------*
      *
      *    IF  WRK-CROTNA-ORIGE        EQUAL SPACES
      *        MOVE 'S'                TO WRK-IND-ERRO-DETALHE
      *        MOVE  1088              TO WRK-COD-MENSAGEM
      *        GO TO 353300-99-FIM
      *    END-IF.
      *
      *    MOVE WRK-CROTNA-ORIGE       TO CROTNA-ORIGE-CONTR
      *                                                    OF GFCTB059.
      *    MOVE GFCT00-DSC-CONTRATO-DESC OF GFCTMVTA
      *                                TO CIDTFD-CONTR-FLEXZ
      *                                                    OF GFCTB059.
      *
      *    EXEC SQL
      *      SELECT
      *            CIDTFD_CONTR_FLEXZ,
      *            CROTNA_ORIGE_CONTR
      *      INTO
      *            :GFCTB059.CIDTFD-CONTR-FLEXZ,
      *            :GFCTB059.CROTNA-ORIGE-CONTR
      *      FROM   DB2PRD.FLEXZ_NRO_CONTR
      *      WHERE
      *            CROTNA_ORIGE_CONTR = :GFCTB059.CROTNA-ORIGE-CONTR AND
      *            CIDTFD_CONTR_FLEXZ = :GFCTB059.CIDTFD-CONTR-FLEXZ
      *    END-EXEC.
      *
      *    IF (SQLCODE                 NOT EQUAL ZEROS AND +100) OR
      *       (SQLWARN0                EQUAL 'W'      )
      *        MOVE 8                  TO RETURN-CODE
      *        MOVE 'DB2'              TO ERR-TIPO-ACESSO
      *        MOVE 'FLEXZ_NRO_CONTR ' TO ERR-DBD-TAB
      *        MOVE 'SELECT  '         TO ERR-FUN-COMANDO
      *        MOVE  SQLCODE           TO ERR-SQL-CODE
      *        MOVE  '0100'            TO ERR-LOCAL
      *        MOVE  SPACES            TO ERR-SEGM
      *        PERFORM 999999-PROCESSAR-ROTINA-ERRO
      *    END-IF.
      *
      *    IF  SQLCODE                 EQUAL +100
      *        MOVE 'S'                TO WRK-IND-ERRO-DETALHE
      *        MOVE  1089              TO WRK-COD-MENSAGEM
      *    END-IF.
      *
      *----------------------------------------------------------------*
      *353300-99-FIM.                  EXIT.
      *----------------------------------------------------------------*
           EJECT
      *----------------------------------------------------------------*
       353400-VALIDAR-GP-CONTA         SECTION.
      *----------------------------------------------------------------*

           IF  WRK-AUX-COD-TARIFA      EQUAL ZEROS
               MOVE 'S'                TO WRK-IND-ERRO-DETALHE
               MOVE  1090              TO WRK-COD-MENSAGEM
               GO TO 353400-99-FIM
           END-IF.

           MOVE '.'                    TO WRK-PT1
                                          WRK-PT2.
      *    MOVE WRK-DATA-EVN-AUX       TO DINIC-FLEXZ-AGPTO OF GFCTB049.
      *    MOVE WRK-AUX-COD-TARIFA     TO CSERVC-TARIF      OF GFCTB049.
      *    MOVE GFCT00-DSC-GRUPO-CONTA OF GFCTMVTA
      *                                TO CAGPTO-CTA        OF GFCTB049.
      *
      *    EXEC SQL
      *      SELECT
      *            CSERVC_TARIF,
      *            CAGPTO_CTA,
      *            DFIM_FLEXZ_AGPTO
      *      INTO
      *            :GFCTB049.CSERVC-TARIF,
      *            :GFCTB049.CAGPTO-CTA,
      *            :GFCTB049.DFIM-FLEXZ-AGPTO
      *      FROM   DB2PRD.FLEXZ_AGPTO_CTA
      *      WHERE
      *            DINIC_FLEXZ_AGPTO  = :GFCTB049.DINIC-FLEXZ-AGPTO  AND
      *            CSERVC_TARIF       = :GFCTB049.CSERVC-TARIF       AND
      *            CAGPTO_CTA         = :GFCTB049.CAGPTO-CTA
      *    END-EXEC.
      *
      *    IF (SQLCODE                 NOT EQUAL ZEROS AND +100) OR
      *       (SQLWARN0                EQUAL 'W'      )
      *        MOVE 8                  TO RETURN-CODE
      *        MOVE 'DB2'              TO ERR-TIPO-ACESSO
      *        MOVE 'FLEXZ_AGPTO_CTA ' TO ERR-DBD-TAB
      *        MOVE 'SELECT  '         TO ERR-FUN-COMANDO
      *        MOVE  SQLCODE           TO ERR-SQL-CODE
      *        MOVE  '0110'            TO ERR-LOCAL
      *        MOVE  SPACES            TO ERR-SEGM
      *        PERFORM 999999-PROCESSAR-ROTINA-ERRO
      *    END-IF.
      *
      *    IF  SQLCODE                 EQUAL +100
      *        MOVE 'S'                TO WRK-IND-ERRO-DETALHE
      *        MOVE  1091              TO WRK-COD-MENSAGEM
      *    END-IF.
      *
      *----------------------------------------------------------------*
       353400-99-FIM.                  EXIT.
      *----------------------------------------------------------------*
           EJECT
      *----------------------------------------------------------------*
       353500-VALIDAR-CONTA-EM-SCCE    SECTION.
      *----------------------------------------------------------------*

           MOVE  GFCT00-EVN-AGE-DEB-CRED  OF GFCTMVTA
                                       TO LKW-CJUNC-DEPC.
           MOVE  GFCT00-EVN-CTA-DEB-CRED  OF GFCTMVTA
                                       TO LKW-CTA-CORR.

           MOVE 'SCCE9100'             TO WRK-MODULO.

           CALL WRK-MODULO             USING LKW-AREA.

           IF  RETURN-CODE             NOT EQUAL ZEROS
               MOVE 8                  TO RETURN-CODE
               MOVE LKW-CD-RETORNO     TO WRK-COD-RET-9100
               MOVE WRK-MSG-9100       TO ERR-TEXTO
               PERFORM 999999-PROCESSAR-ROTINA-ERRO
           END-IF.

           IF  LKW-CD-RETORNO          NOT EQUAL ZEROS
               IF  LKW-CD-RETORNO          EQUAL 05  OR
                   LKW-CD-RETORNO          EQUAL 99
                   MOVE 8                  TO RETURN-CODE
                   MOVE LKW-CD-RETORNO     TO WRK-COD-RET-9100
                   MOVE WRK-MSG-9100       TO ERR-TEXTO
                   PERFORM 999999-PROCESSAR-ROTINA-ERRO
               ELSE
                   MOVE 'S'                TO WRK-IND-ERRO-DETALHE
                   MOVE  1144              TO WRK-COD-MENSAGEM
               END-IF
           END-IF.

      *----------------------------------------------------------------*
       353500-99-FIM.                  EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       360000-TESTAR-GRAVACAO          SECTION.
      *----------------------------------------------------------------*

           IF (WRK-INDREG-REJ      EQUAL SPACES)     AND
              (GFCT00-HDR-TP-REG   OF GFCTMVTA
                                   NOT EQUAL  ZEROS AND 9)
               PERFORM 340000-FORMATAR-TAB-GP
           END-IF.


      *----------------------------------------------------------------*
       360000-99-FIM.                  EXIT.
      *----------------------------------------------------------------*
           EJECT

      *----------------------------------------------------------------*
       370000-FORMATA-ARQ-CTRL-SEQ     SECTION.
      *----------------------------------------------------------------*

           MOVE    GFCT05-REG          OF GFCTSEQE
                                       TO
                   GFCT05-REG          OF GFCTSEQS.

           IF      WRK-INDARQ-REJ      EQUAL     'S'
                   MOVE    'N'         TO WRK-IND-MOVTO-OK
           ELSE
                   MOVE    'S'         TO WRK-IND-MOVTO-OK
                   MOVE    WRK-SEQ-ARQ TO GFCT05-SEQ-ARQ
                                       OF GFCTSEQS.

           MOVE    WRK-TRATAMENTO      TO GFCT05-TRATAMENTO
                                       OF GFCTSEQS
           MOVE    WRK-CLIE-CRTT       TO GFCT05-CLIE-CRTT
                                       OF GFCTSEQS
           MOVE    WRK-CT-EVN-RECBD    TO GFCT05-QTD-EVN-RECBD
                                       OF GFCTSEQS
           MOVE    WRK-CT-EVN-OK       TO GFCT05-QTD-EVN-VALDO
                                       OF GFCTSEQS
           MOVE    WRK-IND-MOVTO-OK    TO GFCT05-IND-MOVTO-OK
                                       OF GFCTSEQS

           MOVE WRK-DT1-DD             TO WRK-DT2-DD
           MOVE WRK-DT1-MM             TO WRK-DT2-MM
           MOVE WRK-DT1-AAAA           TO WRK-DT2-AAAA.
           MOVE WRK-DATA2-DDMMAAAA     TO GFCT05-DATA-PROC
                                       OF GFCTSEQS.

           MOVE WRK-HORA2-AUX          TO GFCT05-HORA-PROC
                                       OF GFCTSEQS.

           MOVE    WRK-SEQ-ARQ         TO GFCT05-SEQ-ORIGEM
                                       OF GFCTSEQS
           MOVE   'GFCT1105'           TO GFCT05-ID-PROCESSO
                                       OF GFCTSEQS.

           WRITE   GFCT05-REG          OF GFCTSEQS.

           MOVE WRK-GRAVACAO           TO WRK-OPERACAO.

           PERFORM 115000-TESTAR-FS-GFCTSEQS.

           ADD 1                       TO WRK-CT-GRAV-SEQ.

      *----------------------------------------------------------------*
       370000-99-FIM.                  EXIT.
      *----------------------------------------------------------------*

           EJECT
      *----------------------------------------------------------------*
       375000-FORMATA-TRAILLER-ERROS   SECTION.
      *----------------------------------------------------------------*

           INITIALIZE   GFCT01-TRAILLER.

           MOVE  '99'                  TO GFCT01-TRL-TP-REG

           IF  WRK-INDARQ-REJ          EQUAL            'S'
               MOVE 'S'                TO GFCT01-TRL-IND-ARQ-REJ
           ELSE
               MOVE WRK-INDARQ-REJ     TO GFCT01-TRL-IND-ARQ-REJ
           END-IF.

           MOVE  WRK-CT-LIDOS          TO GFCT01-TRL-QTDE-REG-ENVIO
           MOVE  WRK-CT-GRV-REJ        TO GFCT01-TRL-QTDE-REG-DEVOL
           MOVE  WRK-PERC-DEVOLU       TO GFCT01-TRL-PERC-DEVOLUCAO
           MOVE  WRK-PERC-DEVOLVI      TO GFCT01-TRL-PERC-DEVOLVIDO.

           WRITE GFCT01-REG            OF GFCTMVER.
           MOVE WRK-GRAVACAO           TO WRK-OPERACAO.
           PERFORM 114000-TESTAR-FS-GFCTMVER.

           ADD 1                       TO WRK-CT-GRAV-ERR.

      *----------------------------------------------------------------*
       375000-99-FIM.                  EXIT.
      *----------------------------------------------------------------*
           EJECT
      *----------------------------------------------------------------*
       400000-FINALIZAR                SECTION.
      *----------------------------------------------------------------*

           PERFORM 410000-EMITIR-ESTATISTICAS.

           IF WRK-CTM GREATER ZEROS
              PERFORM 4500-DISPLAY-CORRELATAS
           ELSE
              DISPLAY ' '
              DISPLAY WRK-DISP-CORR-AST-1
              DISPLAY WRK-DISP-CORR-AST-2
              DISPLAY WRK-DISP-CORR-AST-3
              DISPLAY 'MOVIMENTO SEM CONVERSAO DE CORRELATAS'
              DISPLAY WRK-DISP-CORR-AST-1
              DISPLAY WRK-DISP-CORR-AST-2
              DISPLAY WRK-DISP-CORR-AST-3
              DISPLAY ' '
           END-IF.

FAIRES     IF    LNK-MODULO EQUAL 'PERF'
  "              CLOSE GFCTMVTA
  "                    GFCTMVTE
  "                    GFCTSEQE
  "                    GFCTMVOK
  "                    GFCTSEQS
  "                    GFCTMVRJ
  "                    GFCTMVER
  "        ELSE
  "              CLOSE GFCTMVTA
  "                    GFCTSEQE
  "                    GFCTMVOK
  "                    GFCTSEQS
  "                    GFCTMVRJ
  "                    GFCTMVER
FAIRES     END-IF.

           MOVE WRK-FECHAMENTO         TO WRK-OPERACAO.

           PERFORM 110000-TESTAR-FILE-STATUS.

           GOBACK.

      *----------------------------------------------------------------*
       400000-99-FIM.                  EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       410000-EMITIR-ESTATISTICAS      SECTION.
      *----------------------------------------------------------------*

           DISPLAY '****************** GFCT1105 ******************'.
           DISPLAY '*                                            *'.
           DISPLAY '*            ----- FIM NORMAL -----          *'.
           DISPLAY '*                                            *'.
           MOVE WRK-CT-LIDOS-SEQ       TO WRK-MASCARA.

           DISPLAY '* TOTAL  DE LIDOS CTRL SEQ    : ' WRK-MASCARA    '
      -            ' *'.

           DISPLAY '*                                            *'.
           DISPLAY '* TOTAIS DE REGS LIDOS MOVIMENTO :           *'

           MOVE WRK-CT-LIDOS-0         TO WRK-MASCARA.

           DISPLAY '*          TIPO HEADER        : ' WRK-MASCARA    '
      -            ' *'.

           MOVE WRK-CT-LIDOS-1         TO WRK-MASCARA.

           DISPLAY '*          TIPO EVENTO        : ' WRK-MASCARA    '
      -            ' *'.

           MOVE WRK-CT-LIDOS-2         TO WRK-MASCARA.

           DISPLAY '*          TIPO DESCONTO      : ' WRK-MASCARA    '
      -            ' *'.

           MOVE WRK-CT-LIDOS-3         TO WRK-MASCARA.

           DISPLAY '*          TIPO VALOR LIQUIDO : ' WRK-MASCARA    '
      -            ' *'.

           MOVE WRK-CT-LIDOS-4         TO WRK-MASCARA.

           DISPLAY '*          TIPO COMPLEMENTAR  : ' WRK-MASCARA    '
      -            ' *'.

           MOVE WRK-CT-LIDOS-9         TO WRK-MASCARA.

           DISPLAY '*          TIPO TRAILLER      : ' WRK-MASCARA    '
      -            ' *'.

           MOVE WRK-CT-LIDOS           TO WRK-MASCARA.

           DISPLAY '*                 TOTAL LIDOS : ' WRK-MASCARA   '
      -            '*'.
           DISPLAY '*                                            *'.
           DISPLAY '* TOTAIS DE REGS GRAVADOS ARQ OK :           *'.

           MOVE WRK-CT-GRV-OK-0        TO WRK-MASCARA.

           DISPLAY '*          TIPO HEADER        : ' WRK-MASCARA     '
      -            '  *'.

           MOVE WRK-CT-GRV-OK-1        TO WRK-MASCARA.

           DISPLAY '*          TIPO EVENTO        : ' WRK-MASCARA     '
      -            '  *'.

           MOVE WRK-CT-GRV-OK-2        TO WRK-MASCARA.

           DISPLAY '*          TIPO DESCONTO      : ' WRK-MASCARA     '
      -            '  *'.

           MOVE WRK-CT-GRV-OK-3        TO WRK-MASCARA.

           DISPLAY '*          TIPO VALOR LIQUIDO : ' WRK-MASCARA     '
      -            '  *'.

           MOVE WRK-CT-GRV-OK-4        TO WRK-MASCARA.

           DISPLAY '*          TIPO COMPLEMENTAR  : ' WRK-MASCARA     '
      -            '  *'.

           MOVE WRK-CT-GRV-OK-9        TO WRK-MASCARA.

           DISPLAY '*          TIPO TRAILLER      : ' WRK-MASCARA     '
      -            '  *'.

           MOVE WRK-CT-GRAV-OK         TO WRK-MASCARA.

           DISPLAY '*                    TOTAL OK : ' WRK-MASCARA   '
      -            '*'.
           DISPLAY '*                                            *'.
           DISPLAY '* TOTAIS DE REGS GRAVADOS ARQ INCONSISTENTE: *'.

           MOVE WRK-CT-GRV-REJ-0       TO WRK-MASCARA.

           DISPLAY '*          TIPO HEADER        : ' WRK-MASCARA      '
      -            '   *'.

           MOVE WRK-CT-GRV-REJ-1       TO WRK-MASCARA.

           DISPLAY '*          TIPO EVENTO        : ' WRK-MASCARA      '
      -            '   *'.

           MOVE WRK-CT-GRV-REJ-2       TO WRK-MASCARA.

           DISPLAY '*          TIPO DESCONTO      : ' WRK-MASCARA      '
      -            '   *'.

           MOVE WRK-CT-GRV-REJ-3       TO WRK-MASCARA.

           DISPLAY '*          TIPO VALOR LIQUIDO : ' WRK-MASCARA      '
      -            '   *'.

           MOVE WRK-CT-GRV-REJ-4       TO WRK-MASCARA.

           DISPLAY '*          TIPO COMPLEMENTAR  : ' WRK-MASCARA      '
      -            '   *'.

           MOVE WRK-CT-GRV-REJ-9       TO WRK-MASCARA.

           DISPLAY '*          TIPO TRAILLER      : ' WRK-MASCARA      '
      -            '   *'.

           MOVE WRK-CT-GRV-REJ         TO WRK-MASCARA.

           DISPLAY '*                TOTAL INCONS : ' WRK-MASCARA    '
      -            ' *'.
           DISPLAY '*                                            *'.
           DISPLAY '* TOTAIS DE REGS GRAVADOS ARQ ERROS :        *'.

           MOVE WRK-CT-GRAV-ERR        TO WRK-MASCARA.

           DISPLAY '*                TOTAL ERROS  : ' WRK-MASCARA     '
      -            '  *'.
           DISPLAY '*                                            *'.
           MOVE WRK-CT-GRAV-SEQ        TO WRK-MASCARA.

           DISPLAY '* TOTAL DE GRAVADOS CTRL SEQ  : ' WRK-MASCARA    '
      -            ' *'.
           DISPLAY '*                                            *'.
           DISPLAY '**********************************************'.

FAIRES     MOVE WRK-ACU-NACHOU-CLIEV004 TO WRK-MASCARA
FAIRES     DISPLAY '*        TOTAL CLIEV004 NACHOU  : ' WRK-MASCARA
FAIRES     MOVE WRK-ACU-NACHOU-CLIEV007 TO WRK-MASCARA
FAIRES     DISPLAY '*        TOTAL CLIEV007 NACHOU  : ' WRK-MASCARA
FAIRES     MOVE WRK-ACU-NACHOU-CLIEV008 TO WRK-MASCARA
FAIRES     DISPLAY '*        TOTAL CLIEV008 NACHOU  : ' WRK-MASCARA.
FAIRES     MOVE WRK-ACU-ACHOU-CLIEV004  TO WRK-MASCARA
FAIRES     DISPLAY '*        TOTAL CLIEV004 ACHOU   : ' WRK-MASCARA
FAIRES     MOVE WRK-ACU-ACHOU-CLIEV007  TO WRK-MASCARA
FAIRES     DISPLAY '*        TOTAL CLIEV007 ACHOU   : ' WRK-MASCARA
FAIRES     MOVE WRK-ACU-ACHOU-CLIEV008  TO WRK-MASCARA
FAIRES     DISPLAY '*        TOTAL CLIEV008 ACHOU   : ' WRK-MASCARA.

           IF  WRK-INDARQ-REJ          EQUAL 'S'
               MOVE 4                  TO RETURN-CODE
           ELSE
               MOVE ZEROS              TO RETURN-CODE
           END-IF.

      *----------------------------------------------------------------*
       410000-99-FIM.                  EXIT.
      *----------------------------------------------------------------*
           EJECT


      *----------------------------------------------------------------*
       999999-PROCESSAR-ROTINA-ERRO    SECTION.
      *----------------------------------------------------------------*

           MOVE 'GFCT1105'             TO ERR-PGM.

           IF  ERR-TIPO-ACESSO         NOT EQUAL 'DB2'
               MOVE 'APL'              TO ERR-TIPO-ACESSO
           END-IF.

           CALL 'POOL7100'             USING WRK-BATCH
                                             ERRO-AREA
                                             SQLCA.

           GOBACK.

      *----------------------------------------------------------------*
       999999-99-FIM.                  EXIT.
      *----------------------------------------------------------------*
