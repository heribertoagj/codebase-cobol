      *================================================================*
       IDENTIFICATION                  DIVISION.
      *================================================================*

       PROGRAM-ID. DCOM3151.
       AUTHOR.     CARLOS ALBERTO SERNAGLIA
      *================================================================*
      *                    SIS - CONSULTORIA                           *
      *----------------------------------------------------------------*
      *    PROGRAMA....:   DCOM3151                                    *
      *    ANALISTA SIS:   XXXXXXXXXXXXXXXXXXXXXX                      *
      *    ANALISTA DS.:   XXXXXXXXXXXXXXXXXX                          *
      *    DATA........:   29/05/2020                                  *
      *----------------------------------------------------------------*
      *    OBJETIVO....:   EMITIR RELATORIO DE SOLICITACAO DE          *
      *                :   ENCERRAMENTOS DE CONTAS                     *
      *----------------------------------------------------------------*
      *    ARQUIVOS....:                                               *
      *                DDNAME                        INCLUDE/BOOK      *
      *                    CTEC090E                        -           *
      *                    CTECRELA                        -           *
      *----------------------------------------------------------------*
      *    BOOK'S......:                                               *
      *    I#BRAD7C - AREA PARA TRATAMENTO DE ERRO PELA BRAD7100.      *
      *----------------------------------------------------------------*
      *    MODULOS.....:                                               *
      *    BRAD7100 - TRATAMENTO DE ERROS.                             *
      *================================================================*
      *    POOL7600    : FORNECER A DATA E A HORA CORRENTE DO SISTEMA  *
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

           SELECT CTEC090E ASSIGN      TO UT-S-CTEC090E
                      FILE STATUS      IS WRK-FS-CTEC090E.

           SELECT CTECRELA ASSIGN      TO UT-S-CTECRELA
                      FILE STATUS      IS WRK-FS-CTECRELA.

      *================================================================*
       DATA                            DIVISION.
      *================================================================*

      *----------------------------------------------------------------*
       FILE                            SECTION.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *    INPUT:  ARQUIVO DE OPERACOES                                *
      *            ORG. SEQUENCIAL     - LRECL = 250                   *
      *----------------------------------------------------------------*

       FD  CTEC090E
           RECORDING MODE IS F
           LABEL RECORD IS STANDARD
           BLOCK CONTAINS 0 RECORDS.

       COPY 'I#DCOMUP'.

      *----------------------------------------------------------------*
      *    OUTPUT :  RELATORIO DE SOLICITACAO DE ENCERRAMENTO DE CONTAS*
      *              ORG. SEQUENCIAL   -   LRECL = 133                 *
      *----------------------------------------------------------------*

       FD  CTECRELA
           RECORDING MODE IS F
           LABEL RECORD   IS STANDARD
           BLOCK CONTAINS  0 RECORDS.

       01  FD-CTECRELA                 PIC  X(133).

      *================================================================*
       WORKING-STORAGE                 SECTION.
      *================================================================*

      *----------------------------------------------------------------*
       77  FILLER                      PIC  X(050)         VALUE
           '*** INICIO DA WORKING - DCOM3151 ***'.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       77  FILLER                      PIC  X(050)         VALUE
           '*** AREA DE ACUMULADORES ***'.
      *----------------------------------------------------------------*

       77  ACU-LIDOS-CTEC090E          PIC  9(009) COMP-3  VALUE ZEROS.
       77  ACU-GRAVA-CTECRELA          PIC  9(009) COMP-3  VALUE ZEROS.
       77  ACU-LINHAS                  PIC  9(002) COMP-3  VALUE 99.
       77  ACU-PAGINAS                 PIC  9(003) COMP-3  VALUE ZEROS.

      *----------------------------------------------------------------*
       77  FILLER                      PIC  X(050)         VALUE
           '*** AREA DE FILE-STATUS ***'.
      *----------------------------------------------------------------*

       77  WRK-FS-CTEC090E             PIC  X(002)         VALUE SPACES.
       77  WRK-FS-CTECRELA             PIC  X(002)         VALUE SPACES.

       77  WRK-ABERTURA                PIC  X(013)         VALUE
           ' NA ABERTURA'.
       77  WRK-LEITURA                 PIC  X(013)         VALUE
           ' NA  LEITURA'.
       77  WRK-GRAVACAO                PIC  X(013)         VALUE
           ' NA GRAVACAO'.
       77  WRK-FECHAMENTO              PIC  X(013)         VALUE
           'NO FECHAMENTO'.

      *----------------------------------------------------------------*
       77  FILLER                      PIC  X(050)         VALUE
           '*** AREA VARIAVEIS AUXILIARES ***'.
      *----------------------------------------------------------------*

       77  WRK-MASCARA                 PIC  ZZZ.ZZZ.ZZ9    VALUE ZEROS.
       77  WRK-BATCH                   PIC  X(008)         VALUE
           'BATCH'.

      *--- DATA DE SOLICITACAO - DSOLTC-ENCRR-CTA ---------------------*

       01  WRK-DT-DSOLTC-ENCRR-CTA     PIC  X(008)         VALUE SPACES.
       01  FILLER REDEFINES  WRK-DT-DSOLTC-ENCRR-CTA.
           03 WRK-DD-DSOLTC-ENCRR-CTA  PIC  X(002).
           03 WRK-MM-DSOLTC-ENCRR-CTA  PIC  X(002).
           03 WRK-AA-DSOLTC-ENCRR-CTA  PIC  X(004).
      *
       01  WRK-DATA-DSOLTC-ENCRR-CTA   PIC  X(010)         VALUE SPACES.
       01  FILLER REDEFINES WRK-DATA-DSOLTC-ENCRR-CTA.
           03 WRK-DIA-DSOLTC-ENCRR-CTA PIC  X(002).
           03 WRK-DIA-DSOLTC-PTO       PIC  X(001).
           03 WRK-MES-DSOLTC-ENCRR-CTA PIC  X(002).
           03 WRK-MES-DSOLTC-PTO       PIC  X(001).
           03 WRK-ANO-DSOLTC-ENCRR-CTA PIC  X(004).

      *--- DATA FIM ENCERRAMENTO - DPREVT-EFTVC-DSVNC -----------------*

       01  WRK-DT-DPREVT-EFTVC-DSVNC   PIC  X(008)         VALUE SPACES.
       01  FILLER REDEFINES  WRK-DT-DPREVT-EFTVC-DSVNC.
           03 WRK-DD-DPREVT-EFTVC-DSVNC
                                       PIC  X(002).
           03 WRK-MM-DPREVT-EFTVC-DSVNC
                                       PIC  X(002).
           03 WRK-AA-DPREVT-EFTVC-DSVNC
                                       PIC  X(004).
      *
       01  WRK-DATA-DPREVT-EFTVC-DSVNC PIC  X(010)         VALUE SPACES.
       01  FILLER REDEFINES WRK-DATA-DPREVT-EFTVC-DSVNC.
           03 WRK-DIA-DPREVT-EFTVC-DSVNC
                                       PIC  X(002).
           03 WRK-DIA-DPREVT-PTO       PIC  X(001).
           03 WRK-MES-DPREVT-EFTVC-DSVNC
                                       PIC  X(002).
           03 WRK-MES-DPREVT-PTO       PIC  X(001).
           03 WRK-ANO-DPREVT-EFTVC-DSVNC
                                       PIC  X(004).

      *--- NUMERO DO CPF ----------------------------------------------*

       01  WRK-CPF-CNPJ                PIC  9(015)         VALUE ZEROS.
       01  FILLER REDEFINES  WRK-CPF-CNPJ.
4S2511      03 WRK-CPF-CNPJ-PRINC       PIC  X(09).
4S2511*     03 WRK-CPF-CNPJ-PRINC       PIC  9(009).
4S2511      03 WRK-CPF-CNPJ-FILIAL      PIC  X(04).
4S2511*     03 WRK-CPF-CNPJ-FILIAL      PIC  9(004).
           03 WRK-CPF-CNPJ-CTRL        PIC  9(002).

      *--- CHAVE DO PRODUTO -------------------------------------------*

       01  WRK-CHAVE-PRODUTO           PIC  X(017)         VALUE ZEROS.
       01  FILLER REDEFINES  WRK-CHAVE-PRODUTO.
           03 WRK-CENTRO-CUSTO         PIC  X(004).
           03 WRK-DANO-OPER-DESC       PIC  X(004).
           03 WRK-NSEQ-OPER-DESC       PIC  X(009).

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           '*** AREA  DA  POOL7600 ***'.
      *----------------------------------------------------------------*

       01  WRK-DATA-HORA.
           03  WRK-DATA-JULIANA        PIC  9(005) COMP-3  VALUE ZEROS.
           03  WRK-DATA-AAMMDD         PIC  9(007) COMP-3  VALUE ZEROS.
           03  WRK-DATA-SSAAMMDD       PIC  9(009) COMP-3  VALUE ZEROS.
           03  WRK-HORA-HHMMSS         PIC  9(007) COMP-3  VALUE ZEROS.
           03  WRK-HORA-HHMMSSMMMMMM   PIC  9(013) COMP-3  VALUE ZEROS.
           03  WRK-TIMESTAMP.
               05  WRK-ANO-TSTAMP          PIC  9(004)      VALUE ZEROS.
               05  WRK-MES-TSTAMP          PIC  9(002)      VALUE ZEROS.
               05  WRK-DIA-TSTAMP          PIC  9(002)      VALUE ZEROS.
               05  WRK-HOR-TSTAMP          PIC  9(002)      VALUE ZEROS.
               05  WRK-MIN-TSTAMP          PIC  9(002)      VALUE ZEROS.
               05  WRK-SEG-TSTAMP          PIC  9(002)      VALUE ZEROS.
               05  WRK-MIL-TSTAMP          PIC  9(006)      VALUE ZEROS.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           '*** AUXILIAR PARA ERROS DE FILE-STATUS ***'.
      *----------------------------------------------------------------*

       01  WRK-MSG-FS-ERRO.
           05  FILLER                  PIC  X(009)         VALUE
               '*** ERRO '.
           05  WRK-OPERACAO            PIC  X(013)         VALUE SPACES.
           05  FILLER                  PIC  X(012)         VALUE
               ' DO ARQUIVO '.
           05  WRK-NOME-ARQ            PIC  X(008)         VALUE SPACES.
           05  FILLER                  PIC  X(017)         VALUE
               ' - FILE-STATUS = '.
           05  WRK-FILE-STATUS         PIC  X(002)         VALUE SPACES.
           05  FILLER                  PIC  X(004)         VALUE ' ***'.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           '*** AREA PARA CABECALHOS ***'.
      *----------------------------------------------------------------*

       01  WRK-REL1-CABEC1.
           03  FILLER                  PIC  X(001)         VALUE '1'.
           03  WRK-REL1-CB1-DIA        PIC  9(002)         VALUE ZEROS.
           03  FILLER                  PIC  X(001)         VALUE '/'.
           03  WRK-REL1-CB1-MES        PIC  9(002)         VALUE ZEROS.
           03  FILLER                  PIC  X(001)         VALUE '/'.
           03  WRK-REL1-CB1-ANO        PIC  9(004)         VALUE ZEROS.
           03  FILLER                  PIC  X(039)         VALUE SPACES.
           03  FILLER                  PIC  X(012)         VALUE
               'B A N C O'.
           03  FILLER                  PIC  X(018)         VALUE
               'B R A D E S C O'.
           03  FILLER                  PIC  X(045)         VALUE
               'S / A'.
           03  WRK-REL1-CB1-HORA       PIC  9(002)         VALUE ZEROS.
           03  FILLER                  PIC  X(001)         VALUE ':'.
           03  WRK-REL1-CB1-MINU       PIC  9(002)         VALUE ZEROS.
           03  FILLER                  PIC  X(001)         VALUE ':'.
           03  WRK-REL1-CB1-SEGU       PIC  9(002)         VALUE ZEROS.

       01  WRK-REL1-CABEC2.
           03  FILLER                  PIC  X(001)         VALUE SPACES.
           03  FILLER                  PIC  X(052)         VALUE
               'DCOM3151'.
           03  FILLER                  PIC  X(070)         VALUE
               'SISTEMA DE PRODUTOS VINCULADOS'.
           03  FILLER                  PIC  X(005)         VALUE
               'PAG. '.
           03  WRK-REL1-CB2-PAGINA     PIC  Z.ZZ9          VALUE ZEROS.

       01  WRK-REL1-CABEC3.
           03  FILLER                  PIC  X(001)         VALUE SPACES.
           03  FILLER                  PIC  X(014)         VALUE
               'MOVIMENTO DE: '.
           03  WRK-REL1-CB3-DIA-REF    PIC  9(002)         VALUE ZEROS.
           03  FILLER                  PIC  X(001)         VALUE '/'.
           03  WRK-REL1-CB3-MES-REF    PIC  9(002)         VALUE ZEROS.
           03  FILLER                  PIC  X(001)         VALUE '/'.
           03  WRK-REL1-CB3-ANO-REF    PIC  9(004)         VALUE ZEROS.
           03  FILLER                  PIC  X(016)         VALUE SPACES.
           03  FILLER                  PIC  X(050)         VALUE
               'RELATORIO DE SOLICITACAO DE ENCERRAMENTO DE CONTAS'.
           03  FILLER                  PIC  X(042)         VALUE SPACES.

       01  WRK-REL1-CABEC4.
           03  FILLER                  PIC  X(001)         VALUE SPACES.
           03  WRK-TRACO-REL           PIC  X(132)         VALUE SPACES.

       01  WRK-REL1-CABEC5.
           03  FILLER                  PIC  X(003)         VALUE SPACES.
           03  FILLER                  PIC  X(041)         VALUE
               'NR.SOLICITACAO EMPRESA DO  TIPO CONTRATO '.
           03  FILLER                  PIC  X(061)         VALUE
               'NR.SEQUENCIA DO CODIGO  CODIGO'.
           03  FILLER                  PIC  X(006)         VALUE
               'CODIGO'.
           03  FILLER                  PIC  X(022)         VALUE SPACES.

       01  WRK-REL1-CABEC6.
           03  FILLER                  PIC  X(005)         VALUE SPACES.
           03  FILLER                  PIC  X(040)         VALUE
               'ENCERRAMENTO CONTRATADO DA CONTA (NAS)'.
           03  FILLER                  PIC  X(050)         VALUE
               'CONTRATO (NAS)  BANCO AGENCIA CODIGO CONTA'.
           03  FILLER                  PIC  X(034)         VALUE
               'CPF/CNPJ SISTEMA  CHAVE DO PRODUTO'.
           03  FILLER                  PIC  X(004)         VALUE SPACES.

       01  WRK-REL1-CABEC7.
           03  FILLER                  PIC  X(005)         VALUE SPACES.
           03  FILLER                  PIC  X(045)         VALUE
               'SITUACAO    DATA INCLUSAO   DT.EFETIVACAO'.
           03  FILLER                  PIC  X(083)         VALUE SPACES.

       01  WRK-REL1-CABEC8.
           03  FILLER                  PIC  X(003)         VALUE SPACES.
           03  FILLER                  PIC  X(047)         VALUE
               'DESVINCULO  SOLIC.ENCERRADA   DO DESVINCULO'.
           03  FILLER                  PIC  X(083)         VALUE SPACES.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           '*** AREA PARA DETALHES ***'.
      *----------------------------------------------------------------*

       01  WRK-REL1-LINDET1.
           03  FILLER                       PIC  X(001)    VALUE SPACES.
           03  WRK-REL1-LD1-NSOLT-ENCRR     PIC  9(017)    VALUE ZEROS.
           03  FILLER                       PIC  X(001)    VALUE SPACES.
           03  WRK-REL1-LD1-CPSSOA-JURID    PIC  9(010)    VALUE ZEROS.
           03  FILLER                       PIC  X(012)    VALUE SPACES.
           03  WRK-REL1-LD1-CTPO-NEGOC      PIC  9(003)    VALUE ZEROS.
           03  FILLER                       PIC  X(006)    VALUE SPACES.
           03  WRK-REL1-LD1-NSEQ-NEGOC      PIC  9(010)    VALUE ZEROS.
           03  FILLER                       PIC  X(004)    VALUE SPACES.
           03  WRK-REL1-LD1-BANCO           PIC  9(003)    VALUE ZEROS.
           03  FILLER                       PIC  X(002)    VALUE SPACES.
           03  WRK-REL1-LD1-AGENCIA         PIC  9(005)    VALUE ZEROS.
           03  FILLER                       PIC  X(002)    VALUE SPACES.
           03  WRK-REL1-LD1-CONTA           PIC  9(012)    VALUE ZEROS.
           03  FILLER                       PIC  X(001)    VALUE SPACES.
4S2511*    03  WRK-REL1-LD1-CPF-CNPJ        PIC  9(015)    VALUE ZEROS.
4S2511     03  WRK-REL1-LD1-CPF-CNPJ        PIC  X(015)    VALUE SPACES.
           03  FILLER                       PIC  X(004)    VALUE SPACES.
           03  WRK-REL1-LD1-CSIST           PIC  X(004)    VALUE SPACES.
           03  FILLER                       PIC  X(001)    VALUE SPACES.
           03  WRK-REL1-LD1-CHAVE-PRODT     PIC  X(017)    VALUE SPACES.
           03  FILLER                       PIC  X(004)    VALUE SPACES.

       01  WRK-REL1-LINDET2.
           03  FILLER                       PIC  X(002)    VALUE SPACES.
           03  WRK-REL1-LD2-SIT-DESVN       PIC  X(011)    VALUE SPACES.
           03  FILLER                       PIC  X(007)    VALUE SPACES.
           03  WRK-REL1-LD2-DT-SOLICIT      PIC  X(010)    VALUE SPACES.
           03  FILLER                       PIC  X(006)    VALUE SPACES.
           03  WRK-REL1-LD2-DT-ENCERRAM     PIC  X(010)    VALUE SPACES.
           03  FILLER                       PIC  X(089)    VALUE SPACES.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           '*** AREA PARA A POOM0100 **'.
      *----------------------------------------------------------------*

       COPY 'I#BRAD7C'.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           '*** FINAL DA WORKING - DCOM3151 ***'.
      *----------------------------------------------------------------*

      *================================================================*
       PROCEDURE                       DIVISION.
      *================================================================*

      *----------------------------------------------------------------*
      * ROTINA DE PROCEDIMENTOS PRINCIPAIS                             *
      *----------------------------------------------------------------*
       0000-ROTINA-PRINCIPAL           SECTION.
      *----------------------------------------------------------------*

           PERFORM 1000-INICIALIZAR.

           PERFORM 2000-VERIFICAR-VAZIO.

           PERFORM 3000-PROCESSAR      UNTIL
                   WRK-FS-CTEC090E     EQUAL '10'.

           PERFORM 4000-FINALIZAR.

      *----------------------------------------------------------------*
       0000-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      * ROTINA DE PROCEDIMENTOS DE INICIALIZACAO                       *
      *----------------------------------------------------------------*
       1000-INICIALIZAR                SECTION.
      *----------------------------------------------------------------*

           CALL 'BRAD1050'.

           OPEN INPUT  CTEC090E
                OUTPUT CTECRELA.

           CALL  'BRAD7600'         USING  WRK-DATA-HORA.

           MOVE WRK-ABERTURA           TO WRK-OPERACAO.

           PERFORM 1100-TESTAR-FILE-STATUS.

           PERFORM 1200-OBTER-DATA-HORA.

      *----------------------------------------------------------------*
       1000-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      * ROTINA DE TESTE DE FILE-STATUS                                 *
      *----------------------------------------------------------------*
       1100-TESTAR-FILE-STATUS         SECTION.
      *----------------------------------------------------------------*

           PERFORM 1120-TESTAR-FS-CTEC090E.

           PERFORM 1130-TESTAR-FS-CTECRELA.

      *----------------------------------------------------------------*
       1100-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      * TESTAR FILE STATUS DO ARQUIVO CTEC090E                         *
      *----------------------------------------------------------------*
       1120-TESTAR-FS-CTEC090E         SECTION.
      *----------------------------------------------------------------*

           IF  WRK-FS-CTEC090E         NOT EQUAL '00'
               MOVE 'APL'              TO ERR-TIPO-ACESSO
               MOVE 'CTEC090E'         TO WRK-NOME-ARQ
               MOVE WRK-FS-CTEC090E    TO WRK-FILE-STATUS
               MOVE WRK-MSG-FS-ERRO    TO ERR-TEXTO
               PERFORM 9999-ROTINA-ERRO
           END-IF.

      *----------------------------------------------------------------*
       1120-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      * TESTAR FILE STATUS DO ARQUIVO CTECRELA                         *
      *----------------------------------------------------------------*
       1130-TESTAR-FS-CTECRELA         SECTION.
      *----------------------------------------------------------------*

           IF  WRK-FS-CTECRELA         NOT EQUAL '00'
               MOVE 'APL'              TO ERR-TIPO-ACESSO
               MOVE 'CTECRELA'         TO WRK-NOME-ARQ
               MOVE WRK-FS-CTECRELA    TO WRK-FILE-STATUS
               MOVE WRK-MSG-FS-ERRO    TO ERR-TEXTO
               PERFORM 9999-ROTINA-ERRO
           END-IF.

      *----------------------------------------------------------------*
       1130-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      * OBTER DATA E HORA DO SISTEMA PARA FORMATAR CABECALHOS          *
      *----------------------------------------------------------------*
       1200-OBTER-DATA-HORA            SECTION.
      *----------------------------------------------------------------*

           CALL 'BRAD7600'             USING WRK-DATA-HORA.

           MOVE WRK-DIA-TSTAMP         TO WRK-REL1-CB1-DIA.
           MOVE WRK-MES-TSTAMP         TO WRK-REL1-CB1-MES.
           MOVE WRK-ANO-TSTAMP         TO WRK-REL1-CB1-ANO.
           MOVE WRK-HOR-TSTAMP         TO WRK-REL1-CB1-HORA.
           MOVE WRK-MIN-TSTAMP         TO WRK-REL1-CB1-MINU.
           MOVE WRK-SEG-TSTAMP         TO WRK-REL1-CB1-SEGU.

      *----------------------------------------------------------------*
       1200-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      * VERIFICAR SE ARQUIVO DE ENTRADA ESTA VAZIO                     *
      *----------------------------------------------------------------*
       2000-VERIFICAR-VAZIO            SECTION.
      *----------------------------------------------------------------*

           PERFORM 2200-LER-CTEC090E.

           IF  WRK-FS-CTEC090E         EQUAL '10'
               DISPLAY '************* DCOM3151 *************'
               DISPLAY '*                                  *'
               DISPLAY '*      ARQUIVO CTEC090E VAZIO      *'
               DISPLAY '*     PROCESSAMENTO FINALIZADO     *'
               DISPLAY '*                                  *'
               DISPLAY '************* DCOM3151 *************'
               INITIALIZE UP-REGISTRO
               PERFORM 3100-EMITIR-CABECS
           END-IF.

      *----------------------------------------------------------------*
       2000-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      * LEITURA DO ARQUIVO CTEC090E                                    *
      *----------------------------------------------------------------*
       2200-LER-CTEC090E               SECTION.
      *----------------------------------------------------------------*

           READ CTEC090E.

           IF  WRK-FS-CTEC090E         EQUAL '10'
               GO TO 2200-99-FIM
           END-IF.

           MOVE WRK-LEITURA            TO WRK-OPERACAO.

           PERFORM 1120-TESTAR-FS-CTEC090E.

           ADD 1                       TO ACU-LIDOS-CTEC090E.

      *----------------------------------------------------------------*
       2200-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      * PROCESSAMENTO PRINCIPAL DO PROGRAMA                            *
      *----------------------------------------------------------------*
       3000-PROCESSAR                  SECTION.
      *----------------------------------------------------------------*

           IF  ACU-LINHAS              GREATER 60
               PERFORM 3100-EMITIR-CABECS
           END-IF.

           PERFORM 3200-GERAR-DETALHE.

           PERFORM 2200-LER-CTEC090E.

      *----------------------------------------------------------------*
       3000-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      * EMITIR CABECALHOS DO RELATORIO                                 *
      *----------------------------------------------------------------*
       3100-EMITIR-CABECS              SECTION.
      *----------------------------------------------------------------*

           ADD 1                       TO ACU-PAGINAS.

           MOVE ACU-PAGINAS            TO WRK-REL1-CB2-PAGINA.

           MOVE WRK-DIA-TSTAMP         TO WRK-REL1-CB3-DIA-REF.
           MOVE WRK-MES-TSTAMP         TO WRK-REL1-CB3-MES-REF.
           MOVE WRK-ANO-TSTAMP         TO WRK-REL1-CB3-ANO-REF.

           MOVE WRK-GRAVACAO           TO WRK-OPERACAO.

           WRITE FD-CTECRELA           FROM WRK-REL1-CABEC1.
           PERFORM 1130-TESTAR-FS-CTECRELA.

           WRITE FD-CTECRELA           FROM WRK-REL1-CABEC2.
           PERFORM 1130-TESTAR-FS-CTECRELA.

           WRITE FD-CTECRELA           FROM WRK-REL1-CABEC3.
           PERFORM 1130-TESTAR-FS-CTECRELA.

           MOVE  ALL '='               TO   WRK-TRACO-REL.
           WRITE FD-CTECRELA           FROM WRK-REL1-CABEC4.
           PERFORM 1130-TESTAR-FS-CTECRELA.

           WRITE FD-CTECRELA           FROM WRK-REL1-CABEC5.
           PERFORM 1130-TESTAR-FS-CTECRELA.

           WRITE FD-CTECRELA           FROM WRK-REL1-CABEC6.
           PERFORM 1130-TESTAR-FS-CTECRELA.

           WRITE FD-CTECRELA           FROM WRK-REL1-CABEC7.
           PERFORM 1130-TESTAR-FS-CTECRELA.

           WRITE FD-CTECRELA           FROM WRK-REL1-CABEC8.
           PERFORM 1130-TESTAR-FS-CTECRELA.

           MOVE  ALL '='               TO   WRK-TRACO-REL.
           WRITE FD-CTECRELA           FROM WRK-REL1-CABEC4.
           PERFORM 1130-TESTAR-FS-CTECRELA.

           MOVE 9                      TO ACU-LINHAS.

      *----------------------------------------------------------------*
       3100-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      * GERAR LINHA DE DETALHE NO RELATORIO                            *
      *----------------------------------------------------------------*
       3200-GERAR-DETALHE              SECTION.
      *----------------------------------------------------------------*

           MOVE UP-NSOLTC-ENCRR        TO WRK-REL1-LD1-NSOLT-ENCRR.
           MOVE UP-CPSSOA-JURID        TO WRK-REL1-LD1-CPSSOA-JURID.
           MOVE UP-CTPO-NEGOC          TO WRK-REL1-LD1-CTPO-NEGOC.
           MOVE UP-NSEQ-NEGOC          TO WRK-REL1-LD1-NSEQ-NEGOC.
           MOVE UP-CBCO                TO WRK-REL1-LD1-BANCO.
           MOVE UP-CAG-BCRIA           TO WRK-REL1-LD1-AGENCIA.
           MOVE UP-CCTA-CRIA-CLI       TO WRK-REL1-LD1-CONTA.
      *
           MOVE UP-CPF-CNPJ-PRINC      TO WRK-CPF-CNPJ-PRINC.
           MOVE UP-CPF-CNPJ-FILIAL     TO WRK-CPF-CNPJ-FILIAL.
           MOVE UP-CPF-CNPJ-CTRL       TO WRK-CPF-CNPJ-CTRL.
           MOVE WRK-CPF-CNPJ           TO WRK-REL1-LD1-CPF-CNPJ.
      *
           MOVE UP-COD-SIST            TO WRK-REL1-LD1-CSIST.
      *
           MOVE UP-CENTRO-CUSTO        TO WRK-CENTRO-CUSTO.
           MOVE UP-DANO-OPER-DESC      TO WRK-DANO-OPER-DESC.
           MOVE UP-NSEQ-OPER-DESC      TO WRK-NSEQ-OPER-DESC.
           MOVE WRK-CHAVE-PRODUTO      TO WRK-REL1-LD1-CHAVE-PRODT
      *
           EVALUATE UP-SIT-DESVN
               WHEN 'P'
                 MOVE '   PENDENTE'  TO WRK-REL1-LD2-SIT-DESVN
               WHEN 'C'
                 MOVE '  CANCELADO'  TO WRK-REL1-LD2-SIT-DESVN
               WHEN 'E'
                 MOVE '  ENCERRADO'  TO WRK-REL1-LD2-SIT-DESVN
               WHEN 'X'
                 MOVE 'DT EXCEDIDA'  TO WRK-REL1-LD2-SIT-DESVN
           END-EVALUATE.
      *
           MOVE UP-DSOLTC-ENCRR-CTA    TO WRK-DT-DSOLTC-ENCRR-CTA.
           MOVE WRK-DD-DSOLTC-ENCRR-CTA
                                       TO WRK-DIA-DSOLTC-ENCRR-CTA.
           MOVE '.'                    TO WRK-DIA-DSOLTC-PTO.
           MOVE WRK-MM-DSOLTC-ENCRR-CTA
                                       TO WRK-MES-DSOLTC-ENCRR-CTA.
           MOVE '.'                    TO WRK-MES-DSOLTC-PTO.
           MOVE WRK-AA-DSOLTC-ENCRR-CTA
                                       TO WRK-ANO-DSOLTC-ENCRR-CTA.
           MOVE WRK-DATA-DSOLTC-ENCRR-CTA
                                       TO WRK-REL1-LD2-DT-SOLICIT.
      *
           MOVE UP-DPREVT-EFTVC-DSVNC  TO WRK-DT-DPREVT-EFTVC-DSVNC.
           MOVE WRK-DD-DPREVT-EFTVC-DSVNC
                                       TO WRK-DIA-DPREVT-EFTVC-DSVNC.
           MOVE '.'                    TO WRK-DIA-DPREVT-PTO.
           MOVE WRK-MM-DPREVT-EFTVC-DSVNC
                                       TO WRK-MES-DPREVT-EFTVC-DSVNC.
           MOVE '.'                    TO WRK-MES-DPREVT-PTO.
           MOVE WRK-AA-DPREVT-EFTVC-DSVNC
                                       TO WRK-ANO-DPREVT-EFTVC-DSVNC.
           MOVE WRK-DATA-DPREVT-EFTVC-DSVNC
                                       TO WRK-REL1-LD2-DT-ENCERRAM.
      *
           MOVE WRK-GRAVACAO           TO WRK-OPERACAO.

           WRITE FD-CTECRELA           FROM WRK-REL1-LINDET1.
           PERFORM 1130-TESTAR-FS-CTECRELA.

           WRITE FD-CTECRELA           FROM WRK-REL1-LINDET2.
           PERFORM 1130-TESTAR-FS-CTECRELA.

           ADD 1                       TO ACU-GRAVA-CTECRELA.
           ADD 1                       TO ACU-LINHAS.

      *----------------------------------------------------------------*
       3200-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      * PRODEDIMENTOS FINAIS DO PROGRAMA                               *
      *----------------------------------------------------------------*
       4000-FINALIZAR                  SECTION.
      *----------------------------------------------------------------*

           PERFORM 4200-EMITIR-DISPLAY-FINAL.

           CLOSE CTEC090E
                 CTECRELA.

           MOVE WRK-FECHAMENTO         TO WRK-OPERACAO.

           PERFORM 1100-TESTAR-FILE-STATUS.

           STOP RUN.

      *----------------------------------------------------------------*
       4000-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      * EMITIR ESTATISTICAS DE CONTROLE DE QUANTIDADES                 *
      *----------------------------------------------------------------*
       4200-EMITIR-DISPLAY-FINAL       SECTION.
      *----------------------------------------------------------------*

           DISPLAY '******************* DCOM3151 ******************'.
           DISPLAY '*                                             *'.

           MOVE ACU-LIDOS-CTEC090E     TO WRK-MASCARA.

           DISPLAY '* REGISTROS LIDOS    CTEC090E:   ' WRK-MASCARA
                                                               '  *'.
           DISPLAY '*                                             *'.
           MOVE ACU-GRAVA-CTECRELA     TO WRK-MASCARA.

           DISPLAY '* REGISTROS GRAVADOS CTECRELA:   ' WRK-MASCARA
                                                               '  *'.
           DISPLAY '*                                             *'.
           DISPLAY '******************* DCOM3151 ******************'.

      *----------------------------------------------------------------*
       4200-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      * TRATAMENTOS DE ERROS QUANDO PROGRAMA INVALIDO                  *
      *----------------------------------------------------------------*
       9999-ROTINA-ERRO                SECTION.
      *----------------------------------------------------------------*

           MOVE 'DCOM3151'             TO ERR-PGM.

           CALL 'BRAD7100'             USING WRK-BATCH
                                             ERRO-AREA.

           GOBACK.

      *----------------------------------------------------------------*
       9999-99-FIM.                    EXIT.
      *----------------------------------------------------------------*
