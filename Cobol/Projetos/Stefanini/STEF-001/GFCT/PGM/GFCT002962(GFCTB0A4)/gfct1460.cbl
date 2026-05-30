      *================================================================*
       IDENTIFICATION                  DIVISION.
      *================================================================*

       PROGRAM-ID. GFCT1460.
       AUTHOR. ADRIANA BARON.
      *================================================================*
      *                   C P M   S I S T E M A S                      *
      *----------------------------------------------------------------*
      *                                                                *
      *    PROGRAMA     : GFCT1460                                     *
      *    PROGRAMADOR  : ADRIANA BARON                 - CPM/FPOLIS   *
      *    ANALISTA CPM : MICHELLI VENTURA BORGES       - CPM/FPOLIS   *
      *    ANALISTA     : MARCELO CREMM - PROCWORK      - GRUPO 50     *
      *    DATA         : 11/11/2005                                   *
      *                                                                *
      *    OBJETIVO     :                                              *
      *      ATUALIZA STATUS DE SOLICITACAO COM DECURSO DE PRAZO.      *
      *                                                                *
      *    ARQUIVOS     :                                              *
      *      DDNAME                                INCLUDE/BOOK        *
      *      RELATO                                  --------          *
      *                                                                *
      *    BANCO DE DADOS:                                             *
      *     DB2                                                        *
      *      TABLE                               INCLUDE/BOOK          *
      *      DB2PRD.PARM_CTRL_GSTAO                GFCTB0A0            *
      *      DB2PRD.PARM_DATA_PROCM                GFCTB0A1            *
      *      DB2PRD.PDIDO_ESTRN_AG                 GFCTB0A3            *
      *      DB2PRD.PDIDO_ESTRN_CLI                GFCTB0A4            *
      *      DB2PRD.PDIDO_ESTRN_EMPR               GFCTB0A6            *
      *      DB2PRD.PDIDO_ESTRN_GRP                GFCTB0A7            *
      *      DB2PRD.PDIDO_ESTRN_PAB                GFCTB0A8            *
      *      DB2PRD.PDIDO_ESTRN_PSTAL              GFCTB0A9            *
      *      DB2PRD.PDIDO_ESTRN_SGMTO              GFCTB0B0            *
      *                                                                *
      *    MODULOS CHAMADOS:                                           *
      *      POOL0080 - ALOCAR DINAMICAMENTE UM MODULO.                *
      *      POOL0015 - OBTER DIFERENCA DE DIAS ENTRE DUAS DATAS       *
      *      POOL1050 - MODULO PARA CONEXAO COM DB2.                   *
      *      POOL7100 - TRATAMENTO DE ERROS DA APLICACAO.              *
      *      POOL7300 - OBTER INFORMAOES DO JOB                        *
      *      POOL7600 - FORNECER DATA E HORA ATUAL                     *
      *                                                                *
      *================================================================*

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

           SELECT RELATO  ASSIGN       TO UT-S-RELATO
                     FILE STATUS       IS WRK-FS-RELATO.

      *================================================================*
       DATA                            DIVISION.
      *================================================================*

      *----------------------------------------------------------------*
       FILE                            SECTION.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *    OUTPUT :  RELATORIO DE OCORRENCIAS E TOTALIZADOR            *
      *              ORG. SEQUENCIAL   -   LRECL = 0133                *
      *----------------------------------------------------------------*

       FD  RELATO
           RECORDING MODE IS F
           LABEL RECORD IS STANDARD
           BLOCK CONTAINS 0 RECORDS.

       01  REG-RELATO                  PIC X(133).

      *----------------------------------------------------------------*
       WORKING-STORAGE                 SECTION.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       01  FILLER                      PIC X(32)           VALUE
           '*  INICIO DA WORKING GFCT1460  *'.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       01  FILLER                      PIC X(32)           VALUE
           '*        ACUMULADORES          *'.
      *----------------------------------------------------------------*

       01  ACU-PAGINA                  PIC 9(05)           VALUE ZEROS.

       01  ACU-LINHAS                  PIC 9(02)           VALUE 99.

       01  ACU-LIDOS                   PIC 9(09)           VALUE ZEROS.
       01  ACU-OCORRENCIAS             PIC 9(09)           VALUE ZEROS.
       01  ACU-ENCERRADOS              PIC 9(09)           VALUE ZEROS.

      *----------------------------------------------------------------*
       01  FILLER                      PIC X(32)           VALUE
           '*     VARIAVEIS AUXILIARES     *'.
      *----------------------------------------------------------------*

       01  WRK-DPROCM-ATUAL            PIC X(10)           VALUE SPACES.

       01  WRK-BATCH                   PIC X(08)           VALUE
                                                                'BATCH'.

       01  WRK-MODULO                  PIC X(08)           VALUE SPACES.

       01  WRK-FIM-GFCTB0A7            PIC X(01)           VALUE SPACES.

       01  WRK-DATA-AUX1               PIC 9(08)           VALUE ZEROS.

       01  WRK-DATA-AUX.
           03  WRK-DD                  PIC 9(02)           VALUE ZEROS.
           03  FILLER                  PIC X(01)           VALUE '/'.
           03  WRK-MM                  PIC 9(02)           VALUE ZEROS.
           03  FILLER                  PIC X(01)           VALUE '/'.
           03  WRK-AAAA                PIC 9(04)           VALUE ZEROS.

       01  WRK-DATA-INV                PIC 9(08)           VALUE ZEROS.
       01  WRK-DATA-INV-R              REDEFINES WRK-DATA-INV.
           03 WRK-DT-INV-ANO           PIC 9(04).
           03 WRK-DT-INV-MES           PIC 9(02).
           03 WRK-DT-INV-DIA           PIC 9(02).

       01  WRK-DATA-INI                PIC X(10)           VALUE SPACES.
       01  WRK-DATA-FIM                PIC X(10)           VALUE SPACES.

       01  WRK-ARG-AGENCIA             PIC 9(005)          VALUE ZEROS.
       01  WRK-ARG-CONTA               PIC 9(013)          VALUE ZEROS.
       01  WRK-ARG-POSTO               PIC 9(005)          VALUE ZEROS.
ST25X6*01  WRK-ARG-CPF-CNPJ            PIC 9(009)          VALUE ZEROS.
ST25X6*01  WRK-ARG-FILIAL              PIC 9(005)          VALUE ZEROS.
ST25X6 01  WRK-ARG-CPF-CNPJ            PIC X(009)          VALUE SPACES.
ST25X6 01  WRK-ARG-FILIAL              PIC X(004)          VALUE SPACES.
       01  WRK-ARG-CONTROLE            PIC 9(002)          VALUE ZEROS.
       01  WRK-ARG-SEGMENTO            PIC 9(003)          VALUE ZEROS.

      *----------------------------------------------------------------*
       01  FILLER                      PIC X(32)           VALUE
           '*     VARIAVEIS DE NULIDADE    *'.
      *----------------------------------------------------------------*

       01  WRK-GFCTB0B0-NULL.
           03  WRK-CSGMTO-GSTAO-TARIF-NULL
                                       PIC S9(04) COMP     VALUE +0.

      *----------------------------------------------------------------*
       01  FILLER                      PIC X(32)           VALUE
           '*     AREA PARA FILE-STATUS    *'.
      *----------------------------------------------------------------*

       01  WRK-FS-RELATO               PIC X(02)           VALUE SPACES.

       01  WRK-ABERTURA                PIC X(13)           VALUE
           'NA ABERTURA'.
       01  WRK-GRAVACAO                PIC X(13)           VALUE
           'NA GRAVACAO'.
       01  WRK-FECHAMENTO              PIC X(13)           VALUE
           'NO FECHAMENTO'.

      *----------------------------------------------------------------*
       01  FILLER                      PIC X(32)           VALUE
           '*  AREAS DE MENSAGENS DE ERRO  *'.
      *----------------------------------------------------------------*

       01  WRK-MSG-FS.
           03  FILLER                  PIC X(07)           VALUE SPACES.
           03  FILLER                  PIC X(02)           VALUE '* '.
           03  FILLER                  PIC X(05)           VALUE
               'ERRO '.
           03  WRK-OPERACAO            PIC X(13)           VALUE SPACES.
           03  FILLER                  PIC X(12)           VALUE
               ' DO ARQUIVO '.
           03  WRK-ARQUIVO             PIC X(08)           VALUE SPACES.
           03  FILLER                  PIC X(17)           VALUE
               ' - FILE-STATUS = '.
           03  WRK-FS                  PIC X(02)           VALUE SPACES.
           03  FILLER                  PIC X(02)           VALUE ' *'.

       01  WRK-POOL7300-ERRO.
           03  FILLER                  PIC  X(15)          VALUE SPACES.
           03  FILLER                  PIC  X(40)          VALUE
               'POOL7300 - NUMERO DE PARAMETROS INVALIDO'.

       01  WRK-MSG-PRIM-COMMIT.
           03  FILLER                  PIC X(14)           VALUE SPACES.
           03  FILLER                  PIC X(36)           VALUE
               '* CANCELADO APOS O PRIMEIRO COMMIT *'.

       01  WRK-MSG-BRECA.
           03  FILLER                  PIC X(24)           VALUE SPACES.
           03  FILLER                  PIC X(27)           VALUE
               '* PARM LKG-BRECA INVALIDO *'.

       01  WRK-MSG-COMMIT.
           03  FILLER                  PIC X(21)           VALUE SPACES.
           03  FILLER                  PIC X(33)           VALUE
               '* PARM LKG-QTDE-COMMIT INVALIDO *'.

       01  WRK-MSG-PARM.
           03  FILLER                  PIC X(20)           VALUE SPACES.
           03  FILLER                  PIC X(34)           VALUE
               '* PARM OBRIGATORIO NAO INFORMADO *'.

       01  WRK-MSG-RETORNO.
           03  FILLER                  PIC X(05)           VALUE SPACES.
           03  FILLER                  PIC X(51)           VALUE
               '      * ERRO NO ACESSO   POOL0015 - DT. INVALIDA = '.
           03  WRK-RETORNO             PIC 9(02)           VALUE ZEROS.
           03  FILLER                  PIC X(02)           VALUE ' *'.

       01  WRK-MSG-1-POOL1205.
           03  FILLER                  PIC X(05)           VALUE SPACES.
           03  FILLER                  PIC X(51)           VALUE
               '      * ERRO NO ACESSO A POOL1205 - DATA INICIO  = '.
           03  WRK-MSG-1-1205-RET      PIC 9(02)           VALUE ZEROS.
           03  FILLER                  PIC X(02)           VALUE ' *'.

       01  WRK-MSG-2-POOL1205.
           03  FILLER                  PIC X(05)           VALUE SPACES.
           03  FILLER                  PIC X(51)           VALUE
               '      * ERRO NO ACESSO A POOL1205 - DATA FIM     = '.
           03  WRK-MSG-2-1205-RET      PIC 9(02)           VALUE ZEROS.
           03  FILLER                  PIC X(02)           VALUE ' *'.

      *----------------------------------------------------------------*
       01  FILLER                      PIC X(32)           VALUE
           '*    AREA DA POOL0015          *'.
      *----------------------------------------------------------------*

       01  WRK-CAD-DATA-INFERIOR       PIC 9(09)  COMP-3   VALUE ZEROS.
       01  WRK-CAD-DATA-SUPERIOR       PIC 9(09)  COMP-3   VALUE ZEROS.
       01  WRK-QTDE-DIAS               PIC S9(05) COMP-3   VALUE ZEROS.

      *----------------------------------------------------------------*
       01  FILLER                      PIC X(32)           VALUE
           '*     AREA PARA A POOL1205     *'.
      *----------------------------------------------------------------*

       01  WRK-AREA-POOL1205.
           03  WRK-DT-ENVIADA          PIC 9(08) COMP-3    VALUE ZEROS.
           03  WRK-OPCAO               PIC X(01)           VALUE SPACES.
           03  WRK-DT-JULIANA          PIC 9(07) COMP-3    VALUE ZEROS.
           03  WRK-DT-EDITADA          PIC X(10)           VALUE SPACES.
           03  WRK-DT-GREGORI          PIC 9(08)           VALUE ZEROS.
           03  WRK-DIA-SEMANA          PIC X(13)           VALUE SPACES.
           03  WRK-MES-EXTENSO         PIC X(09)           VALUE SPACES.
           03  WRK-DIA-UT-ANTER        PIC 9(08)           VALUE ZEROS.
           03  WRK-DIA-UT-POSTE        PIC 9(08)           VALUE ZEROS.

       01  WRK-MSG-POOL1205            PIC X(50)           VALUE SPACES.

      *----------------------------------------------------------------*
       01  FILLER                      PIC X(32)           VALUE
           '*     AREA PARA POOL7600       *'.
      *----------------------------------------------------------------*

       01  WRK-DATA-HORA.
           03  WRK-DT-JULIANA          PIC 9(05) COMP-3    VALUE ZEROS.
           03  WRK-DT-AAMMDD           PIC 9(07) COMP-3    VALUE ZEROS.
           03  WRK-DT-AAAAMMDD         PIC 9(09) COMP-3    VALUE ZEROS.
           03  WRK-TI-HHMMSS           PIC 9(07) COMP-3    VALUE ZEROS.
           03  WRK-TI-HHMMSSMMMMMM     PIC 9(13) COMP-3    VALUE ZEROS.
           03  WRK-TIMESTAMP           PIC X(20)           VALUE SPACES.

      *----------------------------------------------------------------*
       01  FILLER                      PIC X(32)        VALUE
           '*       AREA DA POOL7300       *'.
      *----------------------------------------------------------------*

       01  WRK-AREA-POOL7300.
           03  WRK-AMBIENTE            PIC X(05)        VALUE SPACES.
           03  WRK-NOME-JOB            PIC X(08)        VALUE SPACES.
           03  WRK-PROGRAMA            PIC X(08)        VALUE SPACES.
           03  WRK-TIPO-REGIAO         PIC X(03)        VALUE SPACES.
           03  WRK-NOME-PSB            PIC X(08)        VALUE SPACES.
           03  WRK-CODIGO-TRANSACAO    PIC X(08)        VALUE SPACES.
           03  WRK-BACKOUT-DINAMICO    PIC X(01)        VALUE SPACES.
           03  WRK-OPCAO-DBRC          PIC X(01)        VALUE SPACES.
           03  WRK-SUB-SISTEMA         PIC X(04)        VALUE SPACES.
           03  WRK-RESERVA             PIC X(34)        VALUE SPACES.

      *----------------------------------------------------------------*
       01  FILLER                      PIC X(32)           VALUE
           '*      LINHAS DE CABECALHO     *'.
      *----------------------------------------------------------------*

       01  R1-CABEC1.
           03  R1-CB1-CARRO            PIC X(01)           VALUE '1'.
           03  R1-CB1-DATA             PIC X(10)           VALUE SPACES.
           03  FILLER                  PIC X(50)           VALUE
               '                                        B A N C O '.
           03  FILLER                  PIC X(50)           VALUE
               '  B R A D E S C O   S/A                           '.
           03  FILLER                  PIC X(14)           VALUE
               '              '.
           03  R1-CB1-HORA             PIC X(08)           VALUE SPACES.

       01  R1-CABEC2.
           03  R1-CB2-CARRO            PIC X(01)           VALUE ' '.
           03  FILLER                  PIC X(50)           VALUE
               'GFCT1460                               GFCT - GEST'.
           03  FILLER                  PIC X(50)           VALUE
               'AO, FLEXIBILIZACAO E COBRANCA DE TARIFAS          '.
           03  FILLER                  PIC X(28)           VALUE
               '                        PAG.'.
           03  R1-CB2-PAGINA           PIC ZZ99.

       01  R1-CABEC3.
           03  R1-CB3-CARRO            PIC X(01)           VALUE ' '.
           03  FILLER                  PIC X(51)           VALUE
               '                          RELATORIO DE SOLICITACOES'.
           03  FILLER                  PIC X(48)           VALUE
               ' DE CANCELAMENTO ENCERRADAS POR DECURSO DE PRAZO'.

       01  R1-CABEC4.
           03  R1-CB4-CARRO            PIC X(01)           VALUE ' '.
           03  FILLER                  PIC X(50)           VALUE
               '--------------------------------------------------'.
           03  FILLER                  PIC X(50)           VALUE
               '--------------------------------------------------'.
           03  FILLER                  PIC X(32)           VALUE
               '--------------------------------'.

       01  R1-CABEC5.
           03  R1-CB5-CARRO            PIC X(01)           VALUE ' '.
           03  FILLER                  PIC X(50)           VALUE
               '                                                  '.
           03  FILLER                  PIC X(13)           VALUE
               'MOVIMENTO DE '.
           03  R1-CB5-DATA-MOVTO       PIC X(10)           VALUE SPACES.

       01  R1-CABEC6.
           03  R1-CB6-CARRO            PIC X(01)           VALUE '0'.
           03  FILLER                  PIC X(63)           VALUE SPACES.
           03  FILLER                  PIC X(36)           VALUE
               'DATA           DEPENDENCIA          '.
           03  FILLER                  PIC X(33)           VALUE
               'DATA                             '.

       01  R1-CABEC7.
           03  R1-CB7-CARRO            PIC X(01)           VALUE ' '.
           03  FILLER                  PIC X(44)           VALUE
               'AGRUPAMENTO      ARGUMENTO                  '.
           03  FILLER                  PIC X(34)           VALUE
               'TARIFA          DO EVENTO         '.
           03  FILLER                  PIC X(38)           VALUE
               'SOLICITANTE       DO ESTORNO          '.
           03  FILLER                  PIC X(16)           VALUE
               'VALOR SOLICITADO'.

       01  R1-CABEC8.
           03  R1-CB8-CARRO            PIC X(01)           VALUE '0'.
           03  FILLER                  PIC X(50)           VALUE
               '                                   ---------------'.
           03  FILLER                  PIC X(47)           VALUE
               '------ TOTALIZADOR FINAL ----------------------'.

      *----------------------------------------------------------------*
       01  FILLER                      PIC X(32)           VALUE
           '*       LINHAS DE DETALHE      *'.
      *----------------------------------------------------------------*

       01  R1-LINDET1.
           03  R1-LD1-CARRO            PIC X(01)           VALUE ' '.
           03  FILLER                  PIC X(04)           VALUE SPACES.
           03  R1-LD1-AGRUPAMENTO      PIC Z99.
           03  FILLER                  PIC X(10)           VALUE SPACES.
           03  R1-LD1-ARGUMENTO        PIC X(18).
           03  FILLER                  PIC X(10)           VALUE SPACES.
           03  R1-LD1-TARIFA           PIC ZZZZ9.
           03  FILLER                  PIC X(10)           VALUE SPACES.
           03  R1-LD1-DT-OCOR-EVENTO   PIC X(10)           VALUE SPACES.
           03  FILLER                  PIC X(11)           VALUE SPACES.
           03  R1-LD1-DEPEND-SOLICIT   PIC ZZZ99.
           03  FILLER                  PIC X(10)           VALUE SPACES.
           03  R1-LD1-DT-SOLIC-ESTORNO PIC X(10)           VALUE SPACES.
           03  FILLER                  PIC X(10)           VALUE SPACES.
           03  R1-LD1-VALOR-SOL        PIC Z.ZZZ.ZZZ.ZZ9,99.

      *----------------------------------------------------------------*
       01  FILLER                      PIC X(32)           VALUE
           '*        LINHAS DE TOTAL       *'.
      *----------------------------------------------------------------*

       01  R1-LINTOT1.
           03  R1-LT1-CARRO            PIC X(01)           VALUE '0'.
           03  FILLER                  PIC X(50)           VALUE
               '                                   QUANTIDADE DE S'.
           03  FILLER                  PIC X(34)           VALUE
               'OLICITACOES DO DIA..............: '.
           03  R1-LT1-QTD-SOLIC        PIC Z.ZZZ.ZZZ.ZZ9.

       01  R1-LINTOT2.
           03  R1-LT2-CARRO            PIC X(01)           VALUE ' '.
           03  FILLER                  PIC X(50)           VALUE
               '                                   QUANTIDADE DE R'.
           03  FILLER                  PIC X(34)           VALUE
               'EGISTROS ENCERRADOS.............: '.
           03  R1-LT2-QTD-GRAVA        PIC Z.ZZZ.ZZZ.ZZ9.

      *----------------------------------------------------------------*
       01  FILLER                      PIC X(32)           VALUE
           '*      AREA PARA POOL7100      *'.
      *----------------------------------------------------------------*

       COPY POL7100C.

      *----------------------------------------------------------------*
       01  FILLER                      PIC X(32)           VALUE
           '*     AREAS DE TABELAS DB2     *'.
      *----------------------------------------------------------------*

           EXEC SQL
                INCLUDE SQLCA
           END-EXEC.

           EXEC SQL
                INCLUDE GFCTB0A0
           END-EXEC.

           EXEC SQL
                INCLUDE GFCTB0A1
           END-EXEC.

           EXEC SQL
                INCLUDE GFCTB0A3
           END-EXEC.

           EXEC SQL
                INCLUDE GFCTB0A4
           END-EXEC.

           EXEC SQL
                INCLUDE GFCTB0A6
           END-EXEC.

           EXEC SQL
                INCLUDE GFCTB0A7
           END-EXEC.

           EXEC SQL
                INCLUDE GFCTB0A8
           END-EXEC.

           EXEC SQL
                INCLUDE GFCTB0A9
           END-EXEC.

           EXEC SQL
                INCLUDE GFCTB0B0
           END-EXEC.

           EXEC SQL
                INCLUDE GFCTB0F3
           END-EXEC.

      *----------------------------------------------------------------*
       01  FILLER                      PIC X(32)           VALUE
           '*     AREAS PARA CURSORES      *'.
      *----------------------------------------------------------------*

           EXEC SQL
               DECLARE CSR-GFCTB0A7 CURSOR WITH HOLD FOR
               SELECT  CAGPTO_CTA,
                       CSERVC_TARIF,
                       DOCOR_EVNTO,
                       HIDTFD_ESTRN_AGPTO,
                       CSIT_SOLTC_ESTRN,
                       CSIT_PROCM_ESTRN,
                       CINDCD_AGPTO_TOT,
                       CDEPDC_SOLTC_ESTRN,
                       CFUNC_SOLTC_ESTRN,
                       HSOLTC_ESTRN,
                       CTPO_OPER_MOTVO,
                       CMOTVO_JUSTF,
                       RJUSTF_SOLTC_ESTRN,
                       CDEPDC_PAREC_ESTRN,
                       CFUNC_PAREC_ESTRN,
                       HPAREC_ESTRN,
                       RJUSTF_PAREC_ESTRN,
                       QESTRN_AGPTO,
                       VESTRN_SOLTD,
                       QCANCT_AGPTO,
                       VCANCT_SOLTD,
                       DEFETV_CREDT_ESTRN
               FROM    DB2PRD.PDIDO_ESTRN_GRP
               WHERE   CSIT_PROCM_ESTRN   =  3
                 AND   CSIT_SOLTC_ESTRN   =  1
               FOR UPDATE OF CSIT_PROCM_ESTRN
           END-EXEC.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(32)        VALUE
           '*    FIM DA WORKING GFCT1460   *'.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       LINKAGE                         SECTION.
      *----------------------------------------------------------------*

       01  LKG-PARAMETROS.
           03  LKG-TAMANHO             PIC S9(04) COMP.
           03  LKG-QTDE-COMMIT         PIC  9(07).

      *================================================================*
       PROCEDURE                       DIVISION USING LKG-PARAMETROS.
      *================================================================*

      *----------------------------------------------------------------*
       0000000-ROTINA-PRINCIPAL        SECTION.
      *----------------------------------------------------------------*

           CALL 'POOL1050'.

           PERFORM 1000000-INICIALIZAR.

           PERFORM 2000000-PROCESSAR.

           PERFORM 3000000-FINALIZAR.

      *----------------------------------------------------------------*
       0000000-99-FIM.                 EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       1000000-INICIALIZAR             SECTION.
      *----------------------------------------------------------------*

           IF  LKG-TAMANHO             EQUAL ZEROS
               DISPLAY '***************** GFCT1460 ****************'
               DISPLAY '*                                         *'
               DISPLAY '*     PARM OBRIGATORIO NAO INFORMADO      *'
               DISPLAY '*        PROCESSAMENTO CANCELADO          *'
               DISPLAY '*                                         *'
               DISPLAY '***************** GFCT1460 ****************'
               MOVE 'APL'              TO ERR-TIPO-ACESSO
               MOVE WRK-MSG-PARM       TO ERR-TEXTO
               PERFORM 9999999-ROTINA-ERRO
           END-IF.

           IF (LKG-QTDE-COMMIT         NOT NUMERIC) OR
              (LKG-QTDE-COMMIT         EQUAL ZEROS)
               DISPLAY '***************** GFCT1460 ****************'
               DISPLAY '*                                         *'
               DISPLAY '*     PARM LKG-QTDE-COMMIT INVALIDO       *'
               DISPLAY '*        PROCESSAMENTO CANCELADO          *'
               DISPLAY '*                                         *'
               DISPLAY '***************** GFCT1460 ****************'
               MOVE 'APL'              TO ERR-TIPO-ACESSO
               MOVE WRK-MSG-COMMIT     TO ERR-TEXTO
               PERFORM 9999999-ROTINA-ERRO
           END-IF.

           PERFORM 1100000-ABRIR-ARQUIVOS.

           PERFORM 1200000-ACESSAR-POOL7600.

           PERFORM 1300000-OBTER-PARM-A1.

           PERFORM 1400000-OBTER-PARM-A0.

           PERFORM 1500000-OBTER-NOME-JOB.

           PERFORM 1600000-OBTER-DATAS-LIMITES.

      *----------------------------------------------------------------*
       1000000-99-FIM.                 EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       1100000-ABRIR-ARQUIVOS          SECTION.
      *----------------------------------------------------------------*

           OPEN OUTPUT RELATO.

           MOVE WRK-ABERTURA           TO WRK-OPERACAO.

           PERFORM 1110000-TESTAR-FS-RELATO.

      *----------------------------------------------------------------*
       1100000-99-FIM.                 EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       1110000-TESTAR-FS-RELATO        SECTION.
      *----------------------------------------------------------------*

           IF  WRK-FS-RELATO           NOT EQUAL ZEROS
               MOVE 'RELATO'           TO WRK-ARQUIVO
               MOVE WRK-FS-RELATO      TO WRK-FS
               MOVE 'APL'              TO ERR-TIPO-ACESSO
               MOVE WRK-MSG-FS         TO ERR-TEXTO
               PERFORM 9999999-ROTINA-ERRO
           END-IF.

      *----------------------------------------------------------------*
       1110000-99-FIM.                 EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       1200000-ACESSAR-POOL7600        SECTION.
      *----------------------------------------------------------------*

           CALL 'POOL7600'             USING WRK-DATA-HORA.

           STRING  WRK-TIMESTAMP(07:02) '/'
                   WRK-TIMESTAMP(05:02) '/'
                   WRK-TIMESTAMP(01:04)
           DELIMITED BY SIZE           INTO R1-CB1-DATA.

           STRING  WRK-TIMESTAMP(09:02) ':'
                   WRK-TIMESTAMP(11:02) ':'
                   WRK-TIMESTAMP(13:02)
           DELIMITED BY SIZE           INTO R1-CB1-HORA.

      *----------------------------------------------------------------*
       1200000-99-FIM.                 EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       1300000-OBTER-PARM-A1           SECTION.
      *----------------------------------------------------------------*

           MOVE 'GFCT'                 TO CSIST-PRINC OF GFCTB0A1.

           EXEC SQL
                SELECT CSIST_PRINC,
                       DPROCM_ANTER,
                       DPROCM_ATUAL,
                       DPROCM_PROX
                INTO   :GFCTB0A1.CSIST-PRINC,
                       :GFCTB0A1.DPROCM-ANTER,
                       :GFCTB0A1.DPROCM-ATUAL,
                       :GFCTB0A1.DPROCM-PROX
                FROM   DB2PRD.PARM_DATA_PROCM
                WHERE  CSIST_PRINC     = :GFCTB0A1.CSIST-PRINC
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS) OR
              (SQLWARN0                EQUAL 'W')
               MOVE 'DB2'              TO ERR-TIPO-ACESSO
               MOVE 'PARM_DATA_PROCM'  TO ERR-DBD-TAB
               MOVE 'SELECT'           TO ERR-FUN-COMANDO
               MOVE SQLCODE            TO ERR-SQL-CODE
               MOVE '0001'             TO ERR-LOCAL
               MOVE SPACES             TO ERR-SEGM
               PERFORM 9999999-ROTINA-ERRO
           END-IF.

      *----------------------------------------------------------------*
       1300000-99-FIM.                 EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       1400000-OBTER-PARM-A0           SECTION.
      *----------------------------------------------------------------*

           MOVE DPROCM-ATUAL OF GFCTB0A1
                                       TO WRK-DPROCM-ATUAL.

           EXEC SQL
                SELECT  QMAX_PAREC_ESTRN
                INTO   :GFCTB0A0.QMAX-PAREC-ESTRN
                FROM    DB2PRD.PARM_CTRL_GSTAO
                WHERE   DFIM_VGCIA_GSTOR IN
                       (SELECT MAX(DFIM_VGCIA_GSTOR)
                          FROM DB2PRD.PARM_CTRL_GSTAO
                         WHERE DINIC_VGCIA_GSTOR <= :WRK-DPROCM-ATUAL)
                           AND DINIC_VGCIA_GSTOR IN
                              (SELECT MAX(DINIC_VGCIA_GSTOR)
                                 FROM DB2PRD.PARM_CTRL_GSTAO
                                WHERE DINIC_VGCIA_GSTOR
                                       <= :WRK-DPROCM-ATUAL)
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS) OR
              (SQLWARN0                EQUAL 'W')
               MOVE 'DB2'              TO ERR-TIPO-ACESSO
               MOVE 'PARM_CTRL_GSTAO'  TO ERR-DBD-TAB
               MOVE 'SELECT'           TO ERR-FUN-COMANDO
               MOVE SQLCODE            TO ERR-SQL-CODE
               MOVE '0002'             TO ERR-LOCAL
               MOVE SPACES             TO ERR-SEGM
               PERFORM 9999999-ROTINA-ERRO
           END-IF.

      *----------------------------------------------------------------*
       1400000-99-FIM.                 EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       1500000-OBTER-NOME-JOB          SECTION.
      *----------------------------------------------------------------*

           CALL 'POOL7300'             USING WRK-AREA-POOL7300.

           IF  RETURN-CODE             NOT EQUAL ZEROS
               DISPLAY '***************** GFCT1460 *****************'
               DISPLAY '*                                          *'
               DISPLAY '* POOL7300 - NUMERO DE PARAMETROS INVALIDO *'
               DISPLAY '*                                          *'
               DISPLAY '***************** GFCT1460 *****************'
               MOVE WRK-POOL7300-ERRO  TO ERR-TEXTO
               MOVE 'APL'              TO ERR-TIPO-ACESSO
               PERFORM 9999999-ROTINA-ERRO
           END-IF.

      *----------------------------------------------------------------*
       1500000-99-FIM.                 EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       1600000-OBTER-DATAS-LIMITES     SECTION.
      *----------------------------------------------------------------*

           MOVE SPACES                 TO WRK-AREA-POOL1205.
           INITIALIZE                  WRK-AREA-POOL1205.

           MOVE DPROCM-ATUAL OF GFCTB0A1(07:04)
                                       TO WRK-DT-INV-ANO.
           MOVE DPROCM-ATUAL OF GFCTB0A1(04:02)
                                       TO WRK-DT-INV-MES.
           MOVE 01                     TO WRK-DT-INV-DIA.

           MOVE WRK-DATA-INV           TO WRK-DT-ENVIADA.

           CALL 'POOL1205'             USING WRK-AREA-POOL1205
                                             WRK-MSG-POOL1205.

           IF  RETURN-CODE             NOT EQUAL ZEROS OR
               WRK-OPCAO               EQUAL 'I'
               MOVE RETURN-CODE        TO WRK-MSG-1-1205-RET

               DISPLAY '************ GFCT1460 ************'
               DISPLAY '*                                *'
               DISPLAY '*   ERRO NO ACESSO A POOL1205    *'
               DISPLAY '*                                *'
               DISPLAY '*       RETURN-CODE = ' WRK-MSG-1-1205-RET
                                                '        *'
               DISPLAY '*                                *'
               DISPLAY '*       PROGRAMA CANCELADO       *'
               DISPLAY '*                                *'
               DISPLAY '*********** GFCT1460 ************'

               MOVE 'APL'              TO ERR-TIPO-ACESSO

               MOVE WRK-MSG-1-POOL1205 TO ERR-TEXTO

               PERFORM  9999999-ROTINA-ERRO
           END-IF.

           IF  WRK-OPCAO               EQUAL 'U'
               MOVE WRK-DT-EDITADA     TO WRK-DATA-INI
               INSPECT WRK-DATA-INI    REPLACING ALL '/' BY '.'
           ELSE
               STRING WRK-DIA-UT-POSTE(01:02) '.'
                      WRK-DIA-UT-POSTE(03:02) '.'
                      WRK-DIA-UT-POSTE(05:04)
               DELIMITED BY SIZE       INTO WRK-DATA-INI
           END-IF.

           MOVE SPACES                 TO WRK-AREA-POOL1205.
           INITIALIZE                  WRK-AREA-POOL1205.

           IF  WRK-DT-INV-MES          EQUAL 12
               ADD 1                   TO WRK-DT-INV-ANO
               MOVE 01                 TO WRK-DT-INV-MES
           ELSE
               ADD 1                   TO WRK-DT-INV-MES
           END-IF.

           MOVE WRK-DATA-INV           TO WRK-DT-ENVIADA.

           CALL 'POOL1205'             USING WRK-AREA-POOL1205
                                             WRK-MSG-POOL1205.

           IF  RETURN-CODE             NOT EQUAL ZEROS OR
               WRK-OPCAO               EQUAL 'I'
               MOVE RETURN-CODE        TO WRK-MSG-2-1205-RET

               DISPLAY '************ GFCT1460 ************'
               DISPLAY '*                                *'
               DISPLAY '*   ERRO NO ACESSO A POOL1205    *'
               DISPLAY '*                                *'
               DISPLAY '*       RETURN-CODE = ' WRK-MSG-2-1205-RET
                                                '        *'
               DISPLAY '*                                *'
               DISPLAY '*       PROGRAMA CANCELADO       *'
               DISPLAY '*                                *'
               DISPLAY '*********** GFCT1460 ************'

               MOVE 'APL'              TO ERR-TIPO-ACESSO

               MOVE WRK-MSG-2-POOL1205 TO ERR-TEXTO

               PERFORM  9999999-ROTINA-ERRO
           END-IF.

           STRING WRK-DIA-UT-ANTER(01:02) '.'
                  WRK-DIA-UT-ANTER(03:02) '.'
                  WRK-DIA-UT-ANTER(05:04)
           DELIMITED BY SIZE           INTO WRK-DATA-FIM.

      *----------------------------------------------------------------*
       1600000-99-FIM.                 EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       2000000-PROCESSAR               SECTION.
      *----------------------------------------------------------------*

           PERFORM 2100000-ABRIR-CSR-GFCTB0A7.

           PERFORM 2200000-LER-CSR-GFCTB0A7.

           IF  WRK-FIM-GFCTB0A7        EQUAL 'S'
               DISPLAY '*************** GFCT1460 ****************'
               DISPLAY '*                                       *'
               DISPLAY '*    ** NAO  HA  SOLICITACOES DE **     *'
               DISPLAY '*    ** ESTORNO A SER VERIFICADO **     *'
               DISPLAY '*    ** SE EXISTE DECURSO DE PRAZO      *'
               DISPLAY '*    ** MOVIMENTO: ' DPROCM-ATUAL OF GFCTB0A1
                       '    **     *'
               DISPLAY '*                                       *'
               DISPLAY '*************** GFCT1460 ****************'
           END-IF.

           PERFORM 2300000-PROCESSAR-CSR-GFCTB0A7 UNTIL
                      WRK-FIM-GFCTB0A7 EQUAL 'S'.

           PERFORM 2400000-FECHAR-CSR-GFCTB0A7.

      *----------------------------------------------------------------*
       2000000-99-FIM.                 EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       2100000-ABRIR-CSR-GFCTB0A7      SECTION.
      *----------------------------------------------------------------*

           MOVE 'N'                    TO WRK-FIM-GFCTB0A7.

           EXEC SQL
                OPEN CSR-GFCTB0A7
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS) OR
              (SQLWARN0                EQUAL 'W')
               MOVE 'DB2'              TO ERR-TIPO-ACESSO
               MOVE 'PDIDO_ESTRN_GRP'  TO ERR-DBD-TAB
               MOVE 'OPEN'             TO ERR-FUN-COMANDO
               MOVE SQLCODE            TO ERR-SQL-CODE
               MOVE '0003'             TO ERR-LOCAL
               MOVE SPACES             TO ERR-SEGM
               PERFORM 9999999-ROTINA-ERRO
           END-IF.

      *----------------------------------------------------------------*
       2100000-99-FIM.                 EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       2200000-LER-CSR-GFCTB0A7        SECTION.
      *----------------------------------------------------------------*

           EXEC SQL
                FETCH CSR-GFCTB0A7
                INTO  :GFCTB0A7.CAGPTO-CTA,
                      :GFCTB0A7.CSERVC-TARIF,
                      :GFCTB0A7.DOCOR-EVNTO,
                      :GFCTB0A7.HIDTFD-ESTRN-AGPTO,
                      :GFCTB0A7.CSIT-SOLTC-ESTRN,
                      :GFCTB0A7.CSIT-PROCM-ESTRN,
                      :GFCTB0A7.CINDCD-AGPTO-TOT,
                      :GFCTB0A7.CDEPDC-SOLTC-ESTRN,
                      :GFCTB0A7.CFUNC-SOLTC-ESTRN,
                      :GFCTB0A7.HSOLTC-ESTRN,
                      :GFCTB0A7.CTPO-OPER-MOTVO,
                      :GFCTB0A7.CMOTVO-JUSTF,
                      :GFCTB0A7.RJUSTF-SOLTC-ESTRN,
                      :GFCTB0A7.CDEPDC-PAREC-ESTRN,
                      :GFCTB0A7.CFUNC-PAREC-ESTRN,
                      :GFCTB0A7.HPAREC-ESTRN,
                      :GFCTB0A7.RJUSTF-PAREC-ESTRN,
                      :GFCTB0A7.QESTRN-AGPTO,
                      :GFCTB0A7.VESTRN-SOLTD,
                      :GFCTB0A7.QCANCT-AGPTO,
                      :GFCTB0A7.VCANCT-SOLTD,
                      :GFCTB0A7.DEFETV-CREDT-ESTRN
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS AND +100) OR
              (SQLWARN0                EQUAL 'W')
               MOVE 'DB2'              TO ERR-TIPO-ACESSO
               MOVE 'PDIDO_ESTRN_GRP'  TO ERR-DBD-TAB
               MOVE 'FETCH'            TO ERR-FUN-COMANDO
               MOVE SQLCODE            TO ERR-SQL-CODE
               MOVE '0004'             TO ERR-LOCAL
               MOVE SPACES             TO ERR-SEGM
               PERFORM 9999999-ROTINA-ERRO
           END-IF.

           IF  SQLCODE                 EQUAL +100
               MOVE 'S'                TO WRK-FIM-GFCTB0A7
           ELSE
               ADD 1                   TO ACU-LIDOS
           END-IF.

      *----------------------------------------------------------------*
       2200000-99-FIM.                 EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       2300000-PROCESSAR-CSR-GFCTB0A7  SECTION.
      *----------------------------------------------------------------*

           PERFORM 2310000-OBTER-DIAS-DRECEB.

           PERFORM 2320000-TRATAR-SIT3.

           PERFORM 2200000-LER-CSR-GFCTB0A7.

           IF  WRK-FIM-GFCTB0A7        NOT EQUAL 'S'
               IF  ACU-OCORRENCIAS     EQUAL LKG-QTDE-COMMIT
                   MOVE ZEROS          TO ACU-OCORRENCIAS

                   PERFORM 3200000-EFETUAR-COMMIT
               END-IF
           END-IF.

      *----------------------------------------------------------------*
       2300000-99-FIM.                 EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       2310000-OBTER-DIAS-DRECEB       SECTION.
      *----------------------------------------------------------------*

           STRING HSOLTC-ESTRN OF GFCTB0A7(09:02)
                  HSOLTC-ESTRN OF GFCTB0A7(06:02)
                  HSOLTC-ESTRN OF GFCTB0A7(01:04)
           DELIMITED BY SIZE           INTO WRK-DATA-AUX1.

           MOVE WRK-DATA-AUX1          TO WRK-CAD-DATA-INFERIOR.

           STRING DPROCM-ATUAL OF GFCTB0A1(01:02)
                  DPROCM-ATUAL OF GFCTB0A1(04:02)
                  DPROCM-ATUAL OF GFCTB0A1(07:04)
           DELIMITED BY SIZE           INTO WRK-DATA-AUX1.

           MOVE WRK-DATA-AUX1          TO WRK-CAD-DATA-SUPERIOR.

           CALL 'POOL0015'             USING WRK-CAD-DATA-INFERIOR
                                             WRK-CAD-DATA-SUPERIOR
                                             WRK-QTDE-DIAS.

           IF  RETURN-CODE             EQUAL 4
               MOVE RETURN-CODE        TO WRK-RETORNO

               DISPLAY '*********** GFCT1460 ************'
               DISPLAY '*                               *'
               DISPLAY '*   ERRO NO ACESSO A POOL0015   *'
               DISPLAY '*       COD.RETORNO = ' WRK-RETORNO '        *'
               DISPLAY '*      PROGRAMA CANCELADO       *'
               DISPLAY '*                               *'
               DISPLAY '*********** GFCT1460 ************'

               MOVE 'APL'              TO ERR-TIPO-ACESSO

               MOVE WRK-MSG-RETORNO    TO ERR-TEXTO

               PERFORM  9999999-ROTINA-ERRO
           END-IF.

      *----------------------------------------------------------------*
       2310000-99-FIM.                 EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       2320000-TRATAR-SIT3             SECTION.
      *----------------------------------------------------------------*

           IF  WRK-QTDE-DIAS           NOT LESS QMAX-PAREC-ESTRN
                                                           OF GFCTB0A0
               PERFORM 2321000-CARIMBAR-CCSR-GFCTB0A7

               PERFORM 2322000-IMPRIMIR-RELAT1

               ADD 1                   TO ACU-OCORRENCIAS
           END-IF.

      *----------------------------------------------------------------*
       2320000-99-FIM.                 EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       2321000-CARIMBAR-CCSR-GFCTB0A7  SECTION.
      *----------------------------------------------------------------*

           MOVE 5                      TO CSIT-PROCM-ESTRN OF GFCTB0A7.

           EXEC SQL
             UPDATE DB2PRD.PDIDO_ESTRN_GRP
               SET  CSIT_PROCM_ESTRN  = :GFCTB0A7.CSIT-PROCM-ESTRN
              WHERE CURRENT OF CSR-GFCTB0A7
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS) OR
              (SQLWARN0                EQUAL 'W')
               MOVE 'DB2'              TO ERR-TIPO-ACESSO
               MOVE 'PDIDO_ESTRN_GRP'  TO ERR-DBD-TAB
               MOVE 'UPDATE'           TO ERR-FUN-COMANDO
               MOVE SQLCODE            TO ERR-SQL-CODE
               MOVE '0005'             TO ERR-LOCAL
               MOVE SPACES             TO ERR-SEGM
               PERFORM 9999999-ROTINA-ERRO
           END-IF.

      *----------------------------------------------------------------*
       2321000-99-FIM.                 EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       2322000-IMPRIMIR-RELAT1         SECTION.
      *----------------------------------------------------------------*

           IF  ACU-LINHAS              GREATER 59
               PERFORM 2322100-IMPRIMIR-CABEC-REL
               PERFORM 2322200-IMPRIMIR-CABEC-DET-REL
           END-IF.

           MOVE CAGPTO-CTA OF GFCTB0A7 TO R1-LD1-AGRUPAMENTO.

           EVALUATE CAGPTO-CTA OF GFCTB0A7
              WHEN 4
                  PERFORM 2322400-OBTER-CLIENTE
              WHEN 9
                  PERFORM 2322500-OBTER-SEGMENTO
              WHEN 14
                  PERFORM 2322600-OBTER-AGENCIA
              WHEN 15
                  PERFORM 2322700-OBTER-PAB
              WHEN 16
                  PERFORM 2322800-OBTER-EMPRESA
              WHEN 17
                  PERFORM 2322900-OBTER-POSTAL
           END-EVALUATE.

           MOVE CSERVC-TARIF OF GFCTB0A7
                                       TO R1-LD1-TARIFA.

           STRING DOCOR-EVNTO OF GFCTB0A7(01:02) '/'
                  DOCOR-EVNTO OF GFCTB0A7(04:02) '/'
                  DOCOR-EVNTO OF GFCTB0A7(07:04)
           DELIMITED BY SIZE           INTO R1-LD1-DT-OCOR-EVENTO.

           MOVE CDEPDC-SOLTC-ESTRN OF GFCTB0A7
                                       TO R1-LD1-DEPEND-SOLICIT.

           STRING HSOLTC-ESTRN OF GFCTB0A7(09:02) '/'
                  HSOLTC-ESTRN OF GFCTB0A7(06:02) '/'
                  HSOLTC-ESTRN OF GFCTB0A7(01:04)
           DELIMITED BY SIZE           INTO R1-LD1-DT-SOLIC-ESTORNO.

           MOVE VESTRN-SOLTD OF GFCTB0A7
                                       TO R1-LD1-VALOR-SOL.
           MOVE WRK-GRAVACAO           TO WRK-OPERACAO.

           WRITE REG-RELATO            FROM R1-LINDET1.

           PERFORM 1110000-TESTAR-FS-RELATO.

           ADD 1                       TO ACU-LINHAS
                                          ACU-ENCERRADOS.

      *----------------------------------------------------------------*
       2322000-99-FIM.                 EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       2322100-IMPRIMIR-CABEC-REL      SECTION.
      *----------------------------------------------------------------*

           MOVE WRK-GRAVACAO           TO WRK-OPERACAO.

           WRITE REG-RELATO            FROM R1-CABEC1.

           ADD 1                       TO ACU-PAGINA.

           MOVE ACU-PAGINA             TO R1-CB2-PAGINA

           PERFORM 1110000-TESTAR-FS-RELATO.

           WRITE REG-RELATO            FROM R1-CABEC2.

           PERFORM 1110000-TESTAR-FS-RELATO.

           WRITE REG-RELATO            FROM R1-CABEC3.

           PERFORM 1110000-TESTAR-FS-RELATO.

           WRITE REG-RELATO            FROM R1-CABEC4.

           PERFORM 1110000-TESTAR-FS-RELATO.

           MOVE DPROCM-ATUAL   OF GFCTB0A1(1:2)
                                       TO WRK-DD.
           MOVE DPROCM-ATUAL   OF GFCTB0A1(4:2)
                                       TO WRK-MM.
           MOVE DPROCM-ATUAL   OF GFCTB0A1(7:4)
                                       TO WRK-AAAA.
           MOVE WRK-DATA-AUX           TO R1-CB5-DATA-MOVTO.

           WRITE REG-RELATO            FROM R1-CABEC5.

           PERFORM 1110000-TESTAR-FS-RELATO.

           WRITE REG-RELATO            FROM R1-CABEC4.

           PERFORM 1110000-TESTAR-FS-RELATO.

           MOVE 9                      TO ACU-LINHAS.

      *----------------------------------------------------------------*
       2322100-99-FIM.                 EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       2322200-IMPRIMIR-CABEC-DET-REL  SECTION.
      *----------------------------------------------------------------*

           MOVE WRK-GRAVACAO           TO WRK-OPERACAO.

           WRITE REG-RELATO            FROM R1-CABEC6.

           PERFORM 1110000-TESTAR-FS-RELATO.

           WRITE REG-RELATO            FROM R1-CABEC7.

           PERFORM 1110000-TESTAR-FS-RELATO.

           WRITE REG-RELATO            FROM R1-CABEC4.

           PERFORM 1110000-TESTAR-FS-RELATO.

           ADD 3                       TO ACU-LINHAS.

      *----------------------------------------------------------------*
       2322200-99-FIM.                 EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       2322400-OBTER-CLIENTE           SECTION.
      *----------------------------------------------------------------*

           MOVE CAGPTO-CTA         OF GFCTB0A7
                                       TO CAGPTO-CTA   OF GFCTB0A4.
           MOVE CSERVC-TARIF       OF GFCTB0A7
                                       TO CSERVC-TARIF OF GFCTB0A4.
           MOVE DOCOR-EVNTO        OF GFCTB0A7
                                       TO DOCOR-EVNTO  OF GFCTB0A4.
           MOVE HIDTFD-ESTRN-AGPTO OF GFCTB0A7
                                       TO HIDTFD-ESTRN-AGPTO
                                                       OF GFCTB0A4.

           EXEC SQL
               SELECT  CCGC_CPF,
                       CFLIAL_CGC,
                       CCTRL_CPF_CGC,
ST25X6                 CCGC_CPF_ST,
ST25X6                 CFLIAL_CGC_ST,
ST25X6                 CCTRL_CPF_CGC_ST
               INTO    :GFCTB0A4.CCGC-CPF,
                       :GFCTB0A4.CFLIAL-CGC,
                       :GFCTB0A4.CCTRL-CPF-CGC,
ST25X6                 :GFCTB0A4.CCGC-CPF-ST,
ST25X6                 :GFCTB0A4.CFLIAL-CGC-ST,
ST25X6                 :GFCTB0A4.CCTRL-CPF-CGC-ST
               FROM    DB2PRD.PDIDO_ESTRN_CLI
ST25X6*          WHERE   CAGPTO_CTA         = :GFCTB0A4.CAGPTO-CTA
ST25X6*            AND   CSERVC_TARIF       = :GFCTB0A4.CSERVC-TARIF
ST25X6*            AND   DOCOR_EVNTO        = :GFCTB0A4.DOCOR-EVNTO
ST25X6*            AND   HIDTFD_ESTRN_AGPTO = :GFCTB0A4.HIDTFD-ESTRN-AGPTO
ST25X6           WHERE   CAGPTO_CTA         = :GFCTB0A4.CAGPTO-CTA
ST25X6             AND   CSERVC_TARIF       = :GFCTB0A4.CSERVC-TARIF
ST25X6             AND   DOCOR_EVNTO        = :GFCTB0A4.DOCOR-EVNTO
ST25X6             AND   HIDTFD_ESTRN_AGPTO = 
ST25X6                                      :GFCTB0A4.HIDTFD-ESTRN-AGPTO                                           
ST25X6             AND   CCGC_CPF_ST        = :GFCTB0A4.CCGC-CPF-ST
ST25X6             AND   CFLIAL_CGC_ST      = :GFCTB0A4.CFLIAL-CGC-ST
ST25X6             AND   CCTRL_CPF_CGC_ST   = :GFCTB0A4.CCTRL-CPF-CGC-ST
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS) OR
              (SQLWARN0                EQUAL 'W')
               MOVE 'DB2'              TO ERR-TIPO-ACESSO
               MOVE 'PDIDO_ESTRN_CLI'  TO ERR-DBD-TAB
               MOVE 'SELECT'           TO ERR-FUN-COMANDO
               MOVE SQLCODE            TO ERR-SQL-CODE
               MOVE '0006'             TO ERR-LOCAL
               MOVE SPACES             TO ERR-SEGM
               PERFORM 9999999-ROTINA-ERRO
           END-IF.

ST25X6*    MOVE CCGC-CPF      OF GFCTB0A4
ST25X6*                                TO WRK-ARG-CPF-CNPJ.
ST25X6*    MOVE CFLIAL-CGC    OF GFCTB0A4
ST25X6*                                TO WRK-ARG-FILIAL.
ST25X6*    MOVE CCTRL-CPF-CGC OF GFCTB0A4
ST25X6*                                TO WRK-ARG-CONTROLE.
ST25X6     MOVE CCGC-CPF-ST   OF GFCTB0A4
ST25X6                                 TO WRK-ARG-CPF-CNPJ.
ST25X6     MOVE CFLIAL-CGC-ST OF GFCTB0A4
ST25X6                                 TO WRK-ARG-FILIAL.
ST25X6     MOVE CCTRL-CPF-CGC-ST OF GFCTB0A4
ST25X6                                 TO WRK-ARG-CONTROLE.

           IF  WRK-ARG-FILIAL          EQUAL ZEROS
               STRING WRK-ARG-CPF-CNPJ(01:03) '.'
                      WRK-ARG-CPF-CNPJ(04:03) '.'
                      WRK-ARG-CPF-CNPJ(07:03) '-'
                      WRK-ARG-CONTROLE
               DELIMITED BY SIZE       INTO R1-LD1-ARGUMENTO
           ELSE
               STRING WRK-ARG-CPF-CNPJ(01:02) '.'
                      WRK-ARG-CPF-CNPJ(03:03) '.'
                      WRK-ARG-CPF-CNPJ(06:03) '/'
                      WRK-ARG-FILIAL          '-'
                      WRK-ARG-CONTROLE
               DELIMITED BY SIZE       INTO R1-LD1-ARGUMENTO
           END-IF.

      *----------------------------------------------------------------*
       2322400-99-FIM.                 EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       2322500-OBTER-SEGMENTO          SECTION.
      *----------------------------------------------------------------*

           MOVE CAGPTO-CTA         OF GFCTB0A7
                                       TO CAGPTO-CTA   OF GFCTB0B0.
           MOVE CSERVC-TARIF       OF GFCTB0A7
                                       TO CSERVC-TARIF OF GFCTB0B0.
           MOVE DOCOR-EVNTO        OF GFCTB0A7
                                       TO DOCOR-EVNTO  OF GFCTB0B0.
           MOVE HIDTFD-ESTRN-AGPTO OF GFCTB0A7
                                       TO HIDTFD-ESTRN-AGPTO
                                                       OF GFCTB0B0.

           EXEC SQL
               SELECT  CSGMTO_GSTAO_TARIF
               INTO    :GFCTB0B0.CSGMTO-GSTAO-TARIF
                          :WRK-CSGMTO-GSTAO-TARIF-NULL
               FROM    DB2PRD.PDIDO_ESTRN_SGMTO
               WHERE   CAGPTO_CTA         = :GFCTB0B0.CAGPTO-CTA
                 AND   CSERVC_TARIF       = :GFCTB0B0.CSERVC-TARIF
                 AND   DOCOR_EVNTO        = :GFCTB0B0.DOCOR-EVNTO
                 AND   HIDTFD_ESTRN_AGPTO = :GFCTB0B0.HIDTFD-ESTRN-AGPTO
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS) OR
              (SQLWARN0                EQUAL 'W')
               MOVE 'DB2'              TO ERR-TIPO-ACESSO
               MOVE 'PDIDO_ESTRN_SGMTO'
                                       TO ERR-DBD-TAB
               MOVE 'SELECT'           TO ERR-FUN-COMANDO
               MOVE SQLCODE            TO ERR-SQL-CODE
               MOVE '0008'             TO ERR-LOCAL
               MOVE SPACES             TO ERR-SEGM
               PERFORM 9999999-ROTINA-ERRO
           END-IF.

           IF  WRK-CSGMTO-GSTAO-TARIF-NULL
                                       LESS ZEROS
               MOVE ZEROS              TO WRK-ARG-SEGMENTO
           ELSE
               MOVE CSGMTO-GSTAO-TARIF OF GFCTB0B0
                                       TO WRK-ARG-SEGMENTO
           END-IF.

           MOVE WRK-ARG-SEGMENTO       TO R1-LD1-ARGUMENTO.

      *----------------------------------------------------------------*
       2322500-99-FIM.                 EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       2322600-OBTER-AGENCIA           SECTION.
      *----------------------------------------------------------------*

           MOVE CAGPTO-CTA         OF GFCTB0A7
                                       TO CAGPTO-CTA   OF GFCTB0A3.
           MOVE CSERVC-TARIF       OF GFCTB0A7
                                       TO CSERVC-TARIF OF GFCTB0A3.
           MOVE DOCOR-EVNTO        OF GFCTB0A7
                                       TO DOCOR-EVNTO  OF GFCTB0A3.
           MOVE HIDTFD-ESTRN-AGPTO OF GFCTB0A7
                                       TO HIDTFD-ESTRN-AGPTO
                                                       OF GFCTB0A3.

           EXEC SQL
               SELECT  CDEPDC
               INTO    :GFCTB0A3.CDEPDC
               FROM    DB2PRD.PDIDO_ESTRN_AG
               WHERE   CAGPTO_CTA         = :GFCTB0A3.CAGPTO-CTA
                 AND   CSERVC_TARIF       = :GFCTB0A3.CSERVC-TARIF
                 AND   DOCOR_EVNTO        = :GFCTB0A3.DOCOR-EVNTO
                 AND   HIDTFD_ESTRN_AGPTO = :GFCTB0A3.HIDTFD-ESTRN-AGPTO
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS) OR
              (SQLWARN0                EQUAL 'W')
               MOVE 'DB2'              TO ERR-TIPO-ACESSO
               MOVE 'PDIDO_ESTRN_AG'   TO ERR-DBD-TAB
               MOVE 'SELECT'           TO ERR-FUN-COMANDO
               MOVE SQLCODE            TO ERR-SQL-CODE
               MOVE '0007'             TO ERR-LOCAL
               MOVE SPACES             TO ERR-SEGM
               PERFORM 9999999-ROTINA-ERRO
           END-IF.

           MOVE CDEPDC OF GFCTB0A3     TO WRK-ARG-AGENCIA.

           IF  WRK-ARG-AGENCIA         EQUAL ZEROS
               MOVE 'TODAS'            TO R1-LD1-ARGUMENTO
           ELSE
               MOVE WRK-ARG-AGENCIA    TO R1-LD1-ARGUMENTO
           END-IF.

      *----------------------------------------------------------------*
       2322600-99-FIM.                 EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       2322700-OBTER-PAB               SECTION.
      *----------------------------------------------------------------*

           MOVE CAGPTO-CTA         OF GFCTB0A7
                                       TO CAGPTO-CTA      OF GFCTB0A8.
           MOVE CSERVC-TARIF       OF GFCTB0A7
                                       TO CSERVC-TARIF    OF GFCTB0A8.
           MOVE DOCOR-EVNTO        OF GFCTB0A7
                                       TO DOCOR-EVNTO     OF GFCTB0A8.
           MOVE HIDTFD-ESTRN-AGPTO OF GFCTB0A7
                                       TO HIDTFD-ESTRN-AGPTO
                                                          OF GFCTB0A8.

           EXEC SQL
               SELECT  CDEPDC,
                       CPOSTO_SERVC
               INTO    :GFCTB0A8.CDEPDC,
                       :GFCTB0A8.CPOSTO-SERVC
               FROM    DB2PRD.PDIDO_ESTRN_PAB
               WHERE   CAGPTO_CTA         = :GFCTB0A8.CAGPTO-CTA
                 AND   CSERVC_TARIF       = :GFCTB0A8.CSERVC-TARIF
                 AND   DOCOR_EVNTO        = :GFCTB0A8.DOCOR-EVNTO
                 AND   HIDTFD_ESTRN_AGPTO = :GFCTB0A8.HIDTFD-ESTRN-AGPTO
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS) OR
              (SQLWARN0                EQUAL 'W')
               MOVE 'DB2'              TO ERR-TIPO-ACESSO
               MOVE 'PDIDO_ESTRN_PAB'  TO ERR-DBD-TAB
               MOVE 'SELECT'           TO ERR-FUN-COMANDO
               MOVE SQLCODE            TO ERR-SQL-CODE
               MOVE '0009'             TO ERR-LOCAL
               MOVE SPACES             TO ERR-SEGM
               PERFORM 9999999-ROTINA-ERRO
           END-IF.

           MOVE CDEPDC       OF GFCTB0A8
                                       TO WRK-ARG-AGENCIA.
           MOVE CPOSTO-SERVC OF GFCTB0A8
                                       TO WRK-ARG-POSTO.

           IF  WRK-ARG-POSTO           EQUAL ZEROS
               STRING WRK-ARG-AGENCIA '/'
                      'TODOS'
               DELIMITED BY SIZE       INTO R1-LD1-ARGUMENTO
           ELSE
               STRING WRK-ARG-AGENCIA '/'
                      WRK-ARG-POSTO
               DELIMITED BY SIZE       INTO R1-LD1-ARGUMENTO
           END-IF.

      *----------------------------------------------------------------*
       2322700-99-FIM.                 EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       2322800-OBTER-EMPRESA           SECTION.
      *----------------------------------------------------------------*

           MOVE CAGPTO-CTA         OF GFCTB0A7
                                       TO CAGPTO-CTA   OF GFCTB0A6.
           MOVE CSERVC-TARIF       OF GFCTB0A7
                                       TO CSERVC-TARIF OF GFCTB0A6.
           MOVE DOCOR-EVNTO        OF GFCTB0A7
                                       TO DOCOR-EVNTO  OF GFCTB0A6.
           MOVE HIDTFD-ESTRN-AGPTO OF GFCTB0A7
                                       TO HIDTFD-ESTRN-AGPTO
                                                       OF GFCTB0A6.

           EXEC SQL
               SELECT  CAG_EMPR,
                       CNRO_CTA_DEB
               INTO    :GFCTB0A6.CAG-EMPR,
                       :GFCTB0A6.CNRO-CTA-DEB
               FROM    DB2PRD.PDIDO_ESTRN_EMPR
               WHERE   CAGPTO_CTA         = :GFCTB0A6.CAGPTO-CTA
                 AND   CSERVC_TARIF       = :GFCTB0A6.CSERVC-TARIF
                 AND   DOCOR_EVNTO        = :GFCTB0A6.DOCOR-EVNTO
                 AND   HIDTFD_ESTRN_AGPTO = :GFCTB0A6.HIDTFD-ESTRN-AGPTO
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS) OR
              (SQLWARN0                EQUAL 'W')
               MOVE 'DB2'              TO ERR-TIPO-ACESSO
               MOVE 'PDIDO_ESTRN_AMPR' TO ERR-DBD-TAB
               MOVE 'SELECT'           TO ERR-FUN-COMANDO
               MOVE SQLCODE            TO ERR-SQL-CODE
               MOVE '0010'             TO ERR-LOCAL
               MOVE SPACES             TO ERR-SEGM
               PERFORM 9999999-ROTINA-ERRO
           END-IF.

           MOVE CAG-EMPR     OF GFCTB0A6
                                       TO WRK-ARG-AGENCIA.
           MOVE CNRO-CTA-DEB OF GFCTB0A6
                                       TO WRK-ARG-CONTA.

           STRING WRK-ARG-AGENCIA '/'
                  WRK-ARG-CONTA
           DELIMITED BY SIZE           INTO R1-LD1-ARGUMENTO.

      *----------------------------------------------------------------*
       2322800-99-FIM.                 EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       2322900-OBTER-POSTAL            SECTION.
      *----------------------------------------------------------------*

           MOVE CAGPTO-CTA         OF GFCTB0A7
                                       TO CAGPTO-CTA   OF GFCTB0A9.
           MOVE CSERVC-TARIF       OF GFCTB0A7
                                       TO CSERVC-TARIF OF GFCTB0A9.
           MOVE DOCOR-EVNTO        OF GFCTB0A7
                                       TO DOCOR-EVNTO  OF GFCTB0A9.
           MOVE HIDTFD-ESTRN-AGPTO OF GFCTB0A7
                                       TO HIDTFD-ESTRN-AGPTO
                                                       OF GFCTB0A9.

           EXEC SQL
               SELECT  CDEPDC,
                       CPOSTO_SERVC
               INTO    :GFCTB0A9.CDEPDC,
                       :GFCTB0A9.CPOSTO-SERVC
               FROM    DB2PRD.PDIDO_ESTRN_PSTAL
               WHERE   CAGPTO_CTA         = :GFCTB0A9.CAGPTO-CTA
                 AND   CSERVC_TARIF       = :GFCTB0A9.CSERVC-TARIF
                 AND   DOCOR_EVNTO        = :GFCTB0A9.DOCOR-EVNTO
                 AND   HIDTFD_ESTRN_AGPTO = :GFCTB0A9.HIDTFD-ESTRN-AGPTO
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS) OR
              (SQLWARN0                EQUAL 'W')
               MOVE 'DB2'              TO ERR-TIPO-ACESSO
               MOVE 'PDIDO_ESTRN_PSTAL'
                                       TO ERR-DBD-TAB
               MOVE 'SELECT'           TO ERR-FUN-COMANDO
               MOVE SQLCODE            TO ERR-SQL-CODE
               MOVE '0011'             TO ERR-LOCAL
               MOVE SPACES             TO ERR-SEGM
               PERFORM 9999999-ROTINA-ERRO
           END-IF.

           MOVE CDEPDC       OF GFCTB0A9
                                       TO WRK-ARG-AGENCIA.
           MOVE CPOSTO-SERVC OF GFCTB0A9
                                       TO WRK-ARG-POSTO.

           IF  WRK-ARG-POSTO           EQUAL ZEROS
               STRING WRK-ARG-AGENCIA '/'
                      'TODOS'
               DELIMITED BY SIZE       INTO R1-LD1-ARGUMENTO
           ELSE
               STRING WRK-ARG-AGENCIA '/'
                      WRK-ARG-POSTO
               DELIMITED BY SIZE       INTO R1-LD1-ARGUMENTO
           END-IF.

      *----------------------------------------------------------------*
       2322900-99-FIM.                 EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       2400000-FECHAR-CSR-GFCTB0A7     SECTION.
      *----------------------------------------------------------------*

           EXEC SQL
                CLOSE CSR-GFCTB0A7
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS) OR
              (SQLWARN0                EQUAL 'W')
               MOVE 'DB2'              TO ERR-TIPO-ACESSO
               MOVE 'PDIDO_ESTRN_GRP'  TO ERR-DBD-TAB
               MOVE 'CLOSE'            TO ERR-FUN-COMANDO
               MOVE SQLCODE            TO ERR-SQL-CODE
               MOVE '0012'             TO ERR-LOCAL
               MOVE SPACES             TO ERR-SEGM
               PERFORM 9999999-ROTINA-ERRO
           END-IF.

      *----------------------------------------------------------------*
       2400000-99-FIM.                 EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       3000000-FINALIZAR               SECTION.
      *----------------------------------------------------------------*

           IF  ACU-LIDOS               GREATER ZEROS
               PERFORM 3100000-TOTALIZAR-RELAT
           END-IF.

           IF  ACU-OCORRENCIAS         NOT EQUAL ZEROS
               PERFORM 3200000-EFETUAR-COMMIT
           END-IF.

           CLOSE RELATO.

           MOVE WRK-FECHAMENTO         TO WRK-OPERACAO.

           PERFORM 1110000-TESTAR-FS-RELATO.

           GOBACK.

      *----------------------------------------------------------------*
       3000000-99-FIM.                 EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       3100000-TOTALIZAR-RELAT         SECTION.
      *----------------------------------------------------------------*

           PERFORM 2322100-IMPRIMIR-CABEC-REL.

           PERFORM 3110000-IMPRIMIR-CABEC-TOT.

           MOVE WRK-GRAVACAO           TO WRK-OPERACAO.

           MOVE ACU-LIDOS              TO R1-LT1-QTD-SOLIC.

           WRITE REG-RELATO            FROM R1-LINTOT1.

           PERFORM 1110000-TESTAR-FS-RELATO.

           MOVE ACU-ENCERRADOS         TO R1-LT2-QTD-GRAVA.

           WRITE REG-RELATO            FROM R1-LINTOT2.

           PERFORM 1110000-TESTAR-FS-RELATO.

      *----------------------------------------------------------------*
       3100000-99-FIM.                 EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       3110000-IMPRIMIR-CABEC-TOT      SECTION.
      *----------------------------------------------------------------*

           MOVE WRK-GRAVACAO           TO WRK-OPERACAO.

           WRITE REG-RELATO            FROM R1-CABEC8.

           PERFORM 1110000-TESTAR-FS-RELATO.

      *----------------------------------------------------------------*
       3110000-99-FIM.                 EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       3200000-EFETUAR-COMMIT          SECTION.
      *----------------------------------------------------------------*

           EXEC SQL
               COMMIT
           END-EXEC.

           IF  SQLCODE                 NOT EQUAL ZEROS
               MOVE 'DB2'              TO ERR-TIPO-ACESSO
               MOVE 'COMMIT'           TO ERR-DBD-TAB
                                          ERR-FUN-COMANDO
               MOVE SQLCODE            TO ERR-SQL-CODE
               MOVE '0014'             TO ERR-LOCAL
               MOVE SPACES             TO ERR-SEGM
               PERFORM 9999999-ROTINA-ERRO
           END-IF.

      *----------------------------------------------------------------*
       3200000-99-FIM.                 EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       9999999-ROTINA-ERRO             SECTION.
      *----------------------------------------------------------------*

           MOVE 'GFCT1460'             TO ERR-PGM.

           CALL 'POOL7100'             USING WRK-BATCH
                                             ERRO-AREA
                                             SQLCA.

           GOBACK.

      *----------------------------------------------------------------*
       9999999-99-FIM.                 EXIT.
      *----------------------------------------------------------------*
