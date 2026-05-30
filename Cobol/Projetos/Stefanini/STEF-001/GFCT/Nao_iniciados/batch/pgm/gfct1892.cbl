      *================================================================*
       IDENTIFICATION                  DIVISION.
      *================================================================*

       PROGRAM-ID. GFCT1892.
       AUTHOR.     TOSHIAKI OKAYAMA FILHO.
      *================================================================*
      *                    C P M  -  S I S T E M A S                   *
      *----------------------------------------------------------------*
      *    PROGRAMA....:   GFCT1892                                    *
      *    PROGRAMADOR.:   TOSHIAKI OKAYAMA FILHO  - CPM PATO BRANCO   *
      *    ANALISTA CPM:   FELIPE ALEX SOMMER      - CPM PATO BRANCO   *
      *    ANALISTA....:   MARIA VALERIA DE S. T.  - PROCWORK / GP 50  *
      *    DATA........:   06/11/2006                                  *
      *----------------------------------------------------------------*
      *    OBJETIVO....:   COMPLETA  OS AGRUPAMENTOS DO  REGISTRO  POR *
      *       EVENTO QUANDO QUEBRAR A AGENCIA / CONTA.                 *
      *----------------------------------------------------------------*
      *    ARQUIVOS....:                                               *
      *                  DDNAME                        INCLUDE/BOOK    *
      *                    EVENTO92                         -          *
      *                    EVENTOAT                         -          *
      *----------------------------------------------------------------*
      *    BCO DE DADOS:                                               *
      *                DB2                                             *
      *                  TABLE                         INCLUDE/BOOK    *
      *                    DB2PRD.V01CLIENTE_AGENCIA     CLIEV004      *
      *                    DB2PRD.V01CTA_POUPANCA        CLIEV007      *
      *                    DB2PRD.V01CTA_CORRENTE        CLIEV008      *
      *                    DB2PRD.TASSOC_SGMTO_CLI       GFCTB0H3      *
LIE   *                    DB2PRD.OBJE_CLIENTES          OBJEB000      *
LIE   *                    DB2PRD.OBJE_CAD_POUPADOR      OBJEB010
      *----------------------------------------------------------------*
      *    BOOK'S......:                                               *
      *    I#GFCTVA - ARQUIVO DE EVENTOS TABELA GFCTB092.              *
      *    I#GFCTRK - AREA PARA ACESSAR MODULO GFCT5564 - OBTER DADOS. *
      *    I#GFCT0M - AREA PARA ACESSAR MODULO GFCT5564 - ERROS.       *
      *    MESUWIM8 - AREA DO MODULO MESU9718.                         *
      *    POL7100C - AREA PARA TRATAMENTO DE ERROS.                   *
      *----------------------------------------------------------------*
      *    MODULOS.....:                                               *
      *    POOL1050 - CONEXAO COM DB2.                                 *
      *    POOL7100 - TRATAMENTO DE ERRO QUANDO PROGRAMA INVALIDO.     *
      *    GFCT5564 - MODULO PARA OBTER CODIGO DO MUNICIPIO E SIGLA DA *
      *               UF DA AGENCIA.                                   *
      *================================================================*
      *    ULTIMA ALTERACAO - 02/02/2007 - VALERIA                     *
      *    ZERAR CAMPOS QUANDO NAO ENCONTRA AGRUPAMENTOS OU RETORNO    *
      *    DO MODULO EH DIFERENTE DE ZEROS                             *
      *================================================================*
      *================================================================*
      *    ULTIMA ALTERACAO: MAR/2007 - VALERIA                        *
      *    ATUALIZACAO DA INC I#GFCTVA                                 *
      *================================================================*
      *================================================================*
      *    ULTIMA ALTERACAO: MAR/2007 - VALERIA                        *
      *    NAO CHAMAR O MODULO MESU9718 QUANDO POSTO = ZEROS           *
      *================================================================*
      *================================================================*
      *    ULTIMA ALTERACAO - 14/12/2007 - EDIVANIA - CPM              *
      *    MELHORIA DE PERFORMANCE: FAZER CHAMADA DINAMICA DO COBOL AOS*
      *    MODULOS.                                                    *
      *    COLOCAR CONDICAO PARA SEPARAR OS REGISTROS DE CONTA POUPANCA*
      *    DOS REGISTROS DE CONTA CORRENTE ANTES DE ACESSAR AS TABELAS.*
      *================================================================*
      *    ULTIMA ALTERACAO - 19/10/2010 - LIETE    - CPM              *
      *    MELHORIA DE PERFORMANCE: TROCAR A LEITURA DA TABELA         *
      *    CLIEV004 PELA OBJEB000 (CONTA CORRENTE) E PELA OBJEB010     *
      *    POUPANCA                                                    *
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

           SELECT EVENTO92 ASSIGN      TO UT-S-EVENTO92
           FILE STATUS                 IS WRK-FS-EVENTO92.

           SELECT EVENTOAT ASSIGN      TO UT-S-EVENTOAT
           FILE STATUS                 IS WRK-FS-EVENTOAT.

      *================================================================*
       DATA                            DIVISION.
      *================================================================*

      *----------------------------------------------------------------*
       FILE                            SECTION.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *    INPUT:  REGISTROS EVENTO DO DIA TABELA GFCTB092             *
      *            ORG. SEQUENCIAL     -   LRECL   =   250             *
      *----------------------------------------------------------------*

       FD  EVENTO92
           RECORDING MODE IS F
           LABEL RECORD IS STANDARD
           BLOCK CONTAINS 0 RECORDS.

       01  REG-EVENTO92                PIC  X(250).

      *----------------------------------------------------------------*
      *   OUTPUT:  REGISTROS EVENTOS ATUALIZADOS GFCTB092              *
      *            ORG. SEQUENCIAL     -   LRECL   =   250             *
      *----------------------------------------------------------------*

       FD  EVENTOAT
           RECORDING MODE IS F
           LABEL RECORD IS STANDARD
           BLOCK CONTAINS 0 RECORDS.

       01  REG-EVENTOAT                PIC  X(250).

      *----------------------------------------------------------------*
       WORKING-STORAGE                 SECTION.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       77  FILLER                      PIC  X(033)         VALUE
           'INICIO DA WORKING STORAGE SECTION'.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       77  FILLER                      PIC  X(012)         VALUE
           'ACUMULADORES'.
      *----------------------------------------------------------------*

       77  ACU-LIDOS-EVENTO92          PIC  9(010) COMP-3  VALUE ZEROS.
       77  ACU-GRAVS-EVENTOAT          PIC  9(010) COMP-3  VALUE ZEROS.
LIE    77  ACU-LIDOS-CLIEV004          PIC  9(010) COMP-3  VALUE ZEROS.
LIE    77  ACU-LIDOS-OBJEB000          PIC  9(010) COMP-3  VALUE ZEROS.
LIE    77  ACU-LIDOS-OBJEB010          PIC  9(010) COMP-3  VALUE ZEROS.

      *----------------------------------------------------------------*
       77  FILLER                      PIC  X(020)         VALUE
           'TESTE DE FILE STATUS'.
      *----------------------------------------------------------------*

       77  WRK-FS-EVENTO92             PIC  X(002)         VALUE SPACES.
       77  WRK-FS-EVENTOAT             PIC  X(002)         VALUE SPACES.

       77  WRK-ABERTURA                PIC  X(013)         VALUE
           ' NA ABERTURA'.
       77  WRK-LEITURA                 PIC  X(013)         VALUE
           '  NA LEITURA'.
       77  WRK-GRAVACAO                PIC  X(013)         VALUE
           ' NA GRAVACAO '.
       77  WRK-FECHAMENTO              PIC  X(013)         VALUE
           'NO FECHAMENTO'.

      *----------------------------------------------------------------*
       77  FILLER                      PIC  X(030)         VALUE
           'AREA PARA VARIAVEIS AUXILIARES'.
      *----------------------------------------------------------------*

       77  WRK-BATCH                   PIC  X(008)         VALUE
           'BATCH'.
       77  WRK-MASCARA                 PIC ZZZZ.ZZZ.999    VALUE ZEROS.
       77  WRK-MODULO                  PIC  X(008)         VALUE SPACES.
       77  WRK-CID-CLI                 PIC  X(026)         VALUE SPACES.
       77  WRK-CSGMTO-CLI              PIC S9(003)V COMP-3 VALUE ZEROS.
       77  WRK-COD-POSTO               PIC S9(003)V COMP-3 VALUE ZEROS.

       77  WRK-BCO-POSTAL              PIC S9(005)V COMP-3 VALUE ZEROS.
       77  WRK-CPAB                    PIC S9(005)V COMP-3 VALUE ZEROS.
       77  WRK-PAB-ANT                 PIC S9(005)V COMP-3 VALUE ZEROS.
LIE    77  WRK-CSGMTO-CLI-ANT          PIC S9(003)V COMP-3 VALUE ZEROS.
LFM    77  WRK-GRUPO                   PIC S9(03)   COMP-3 VALUE ZEROS.
LFM    77  WRK-SUB-GRUPO               PIC S9(03)   COMP-3 VALUE ZEROS.
LIE    77  WRK-ACHOU0                  PIC  X(01)          VALUE SPACES.
LIE    77  WRK-ACHOU4                  PIC  X(01)          VALUE SPACES.
LIE    77  WRK-ACHOU10                 PIC  X(01)          VALUE SPACES.
LIE    77  WRK-ROTINA                  PIC  X(04)          VALUE SPACES.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(016)         VALUE
           'AREA PARA CHAVES'.
      *----------------------------------------------------------------*

       01  WRK-CH-MOVIMENTO-ATU.
           05 WRK-MOV-CAG-DSTNO-MOVTO  PIC S9(005) COMP-3  VALUE ZEROS.
           05 WRK-MOV-CCTA-DSTNO-MOVTO PIC S9(013) COMP-3  VALUE ZEROS.

       01  WRK-CH-MOVIMENTO-ANT.
           05 WRK-MOV-CAG-DSTN         PIC S9(005) COMP-3  VALUE ZEROS.
           05  WRK-MOV-CCTA-DS         PIC S9(013) COMP-3  VALUE ZEROS.

       01  WRK-DATA-ANT.
           05  WRK-DIA-ANT             PIC  9(002)         VALUE ZEROS.
           05  FILLER                  PIC  X(001)         VALUE SPACES.
           05  WRK-MES-ANT             PIC  9(002)         VALUE ZEROS.
           05  FILLER                  PIC  X(001)         VALUE SPACES.
           05  WRK-ANO-ANT             PIC  9(004)         VALUE ZEROS.

       01  WRK-CCTRL-CPF               PIC  9(002)         VALUE ZEROS.
       01  WRK-CCTRL-CPF-R             REDEFINES         WRK-CCTRL-CPF.
           05  WRK-CCTRL-CPF-AUX       PIC  X(002).

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(041)         VALUE
                  '*** AREA PARA MENSAGEM ***'.
      *----------------------------------------------------------------*

       01  WRK-MSG04                   PIC  X(075)         VALUE
           '** ERRO MODULO MESU9718 - INCONSISTENCIA FISICA **'.

       01  WRK-MSG05                   PIC  X(075)         VALUE
           '** ERRO MODULO MESU9718 - INCONSISTENCIA LOGICA **'.

       01  WRK-MSG06                   PIC  X(075)         VALUE
           '** ERRO MODULO MESU9718 - ERRO GENERICO **'.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(041)         VALUE
           'AREA PARA MENSAGEM DE ERRO DE FILE-STATUS'.
      *----------------------------------------------------------------*

       01  WRK-ERRO-ARQUIVO.
           05  FILLER                  PIC  X(008)         VALUE
               '** ERRO '.
           05  WRK-OPERACAO            PIC  X(013)         VALUE SPACES.
           05  FILLER                  PIC  X(012)         VALUE
               ' DO ARQUIVO '.
           05  WRK-NOME-ARQ            PIC  X(008)         VALUE SPACES.
           05  FILLER                  PIC  X(017)         VALUE
               ' - FILE-STATUS = '.
           05  WRK-FILE-STATUS         PIC  X(002)         VALUE SPACES.
           05  FILLER                  PIC  X(003)         VALUE ' **'.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(044)         VALUE
           'AREA PARA ARQUIVO DE EVENTOS TABELA GFCTB092'.
      *----------------------------------------------------------------*

       COPY 'I#GFCTVA'.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(047)         VALUE
           'AREA PARA ACESSAR MODULO GFCT5564 - OBTER DADOS'.
      *----------------------------------------------------------------*

       COPY 'I#GFCTRK'.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(041)         VALUE
           'AREA PARA ACESSAR MODULO GFCT5564 - ERROS'.
      *----------------------------------------------------------------*

       COPY 'I#GFCT0M'.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           '*** AREA PARA COMUNICACAO COM MESU9718 ***'.
      *----------------------------------------------------------------*

       COPY 'I#MESUM8'.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(018)         VALUE
           'AREA PARA POOL7100'.
      *----------------------------------------------------------------*

       COPY 'POL7100C'.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(011)         VALUE
           'AREA DE DB2'.
      *----------------------------------------------------------------*

           EXEC SQL
               INCLUDE SQLCA
           END-EXEC.

           EXEC SQL
               INCLUDE CLIEV004
           END-EXEC.

           EXEC SQL
               INCLUDE CLIEV007
           END-EXEC.

           EXEC SQL
               INCLUDE CLIEV008
           END-EXEC.

           EXEC SQL
               INCLUDE GFCTB0H3
           END-EXEC.

LIE'       EXEC SQL
               INCLUDE OBJEB000
LIE''      END-EXEC.

LIE'       EXEC SQL
               INCLUDE OBJEB010
LIE''      END-EXEC.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(030)         VALUE
           'FIM DA WORKING STORAGE SECTION'.
      *----------------------------------------------------------------*

      *================================================================*
       PROCEDURE                       DIVISION.
      *================================================================*

      ******************************************************************
      *    ROTINA INICIAL DO PROGRAMA.                                 *
      ******************************************************************
      *----------------------------------------------------------------*
       0000-INICIAR                    SECTION.
      *----------------------------------------------------------------*

           PERFORM 1000-INICIALIZAR.

           PERFORM 2000-VERIFICAR-VAZIO.

           PERFORM 3000-PROCESSAR      UNTIL
                   WRK-FS-EVENTO92     EQUAL '10'.

           PERFORM 4000-FINALIZAR.

      *----------------------------------------------------------------*
       0000-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *    ABRIR ARQUIVOS E TESTAR FILE STATUS.                        *
      ******************************************************************
      *----------------------------------------------------------------*
       1000-INICIALIZAR                SECTION.
      *----------------------------------------------------------------*

           CALL 'POOL1050'.

           OPEN INPUT EVENTO92
               OUTPUT EVENTOAT.

           MOVE WRK-ABERTURA           TO WRK-OPERACAO.

           PERFORM 1100-TESTAR-FILE-STATUS.

      *----------------------------------------------------------------*
       1000-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *    TRATAR FILE STATUS DOS ARQUIVOS.                            *
      ******************************************************************
      *----------------------------------------------------------------*
       1100-TESTAR-FILE-STATUS         SECTION.
      *----------------------------------------------------------------*

           PERFORM 1110-TESTAR-FS-EVENTO92.

           PERFORM 1120-TESTAR-FS-EVENTOAT.

      *----------------------------------------------------------------*
       1100-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *    TRATAR FILE STATUS DO ARQUIVO DE ENTRADA EVENTO92.          *
      ******************************************************************
      *----------------------------------------------------------------*
       1110-TESTAR-FS-EVENTO92         SECTION.
      *----------------------------------------------------------------*

           IF  WRK-FS-EVENTO92         NOT EQUAL '00'
               MOVE 'APL'              TO ERR-TIPO-ACESSO
               MOVE 'EVENTO92'         TO WRK-NOME-ARQ
               MOVE WRK-FS-EVENTO92    TO WRK-FILE-STATUS
               MOVE WRK-ERRO-ARQUIVO   TO ERR-TEXTO
               PERFORM 9999-PROCESSAR-ROTINA-ERRO
           END-IF.

      *----------------------------------------------------------------*
       1110-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *    TRATAR FILE STATUS DO ARQUIVO DE SAIDA EVENTOAT.            *
      ******************************************************************
      *----------------------------------------------------------------*
       1120-TESTAR-FS-EVENTOAT         SECTION.
      *----------------------------------------------------------------*

           IF  WRK-FS-EVENTOAT         NOT EQUAL '00'
               MOVE 'APL'              TO ERR-TIPO-ACESSO
               MOVE 'EVENTOAT'         TO WRK-NOME-ARQ
               MOVE WRK-FS-EVENTOAT    TO WRK-FILE-STATUS
               MOVE WRK-ERRO-ARQUIVO   TO ERR-TEXTO
               PERFORM 9999-PROCESSAR-ROTINA-ERRO
           END-IF.

      *----------------------------------------------------------------*
       1120-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *    VERIFICAR SE O ARQUIVO DE ENTRADA EVENTO92 ESTA VAZIO.      *
      ******************************************************************
      *----------------------------------------------------------------*
       2000-VERIFICAR-VAZIO            SECTION.
      *----------------------------------------------------------------*

           PERFORM 2100-LER-EVENTO92.

           IF  WRK-FS-EVENTO92         EQUAL '10'
               DISPLAY '*--------------------------------------------*'
               DISPLAY '*             PROGRAMA GFCT1892              *'
               DISPLAY '*             -----------------              *'
               DISPLAY '*                                            *'
               DISPLAY '*         ARQUIVO DE EVENTO92 VAZIO          *'
               DISPLAY '*--------------------------------------------*'
           END-IF.

      *----------------------------------------------------------------*
       2000-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *    LEITURA DO ARQUIVO DE ENTRADA EVENTO92.                     *
      ******************************************************************
      *----------------------------------------------------------------*
       2100-LER-EVENTO92               SECTION.
      *----------------------------------------------------------------*

           READ EVENTO92.

           IF  WRK-FS-EVENTO92         EQUAL '10'
               MOVE HIGH-VALUES        TO WRK-CH-MOVIMENTO-ANT
               GO TO 2100-99-FIM
           END-IF.

           MOVE WRK-LEITURA            TO WRK-OPERACAO.

           PERFORM 1110-TESTAR-FS-EVENTO92.

           MOVE REG-EVENTO92           TO GFCTVA-GFCTB092.
           MOVE GFCTVA-CAG-DSTNO-MOVTO TO WRK-MOV-CAG-DSTNO-MOVTO.
           MOVE GFCTVA-CCTA-DSTNO-MOVTO TO WRK-MOV-CCTA-DSTNO-MOVTO.

           ADD 1                       TO ACU-LIDOS-EVENTO92.

      *----------------------------------------------------------------*
       2100-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *    PROCESSAMENTO PRINCIPAL DO PROGRAMA.                        *
      ******************************************************************
      *----------------------------------------------------------------*
       3000-PROCESSAR                  SECTION.
      *----------------------------------------------------------------*

           IF  WRK-CH-MOVIMENTO-ANT    NOT EQUAL WRK-CH-MOVIMENTO-ATU
               PERFORM 3100-PESQUISA-AGRUPAMENTOS
           END-IF.

           IF  GFCTVA-CPAB-DSTNO-MOVTO NOT EQUAL WRK-PAB-ANT
               IF  GFCTVA-CPAB-DSTNO-MOVTO NOT EQUAL ZEROS
                   PERFORM 3180-PESQUISA-BCO-POSTAL
                   MOVE GFCTVA-CPAB-DSTNO-MOVTO
                                       TO WRK-PAB-ANT
               ELSE
                   MOVE ZEROS          TO WRK-BCO-POSTAL
                   MOVE ZEROS          TO WRK-CPAB
           END-IF.

           MOVE WRK-BCO-POSTAL         TO GFCTVA-BCO-POSTAL
      ***  MOVE WRK-CPAB               TO GFCTVA-CPAB
           MOVE WRK-COD-POSTO          TO GFCTVA-CPAB.

           MOVE GFCTRK-CMUN-RURAL      TO GFCTVA-CMUN-RURAL.
           MOVE GFCTRK-CSGL-UF         TO GFCTVA-CSGL-UF.

LFM        MOVE  WRK-GRUPO             TO   GFCTVA-GRUPO.
LFM        MOVE  WRK-SUB-GRUPO         TO   GFCTVA-SUB-GRUPO.

LIE        IF    WRK-ACHOU0            EQUAL   'S'
ST25X6           MOVE NUM-CGCCPF-ST    OF  OBJEB000
ST25X6                                 TO GFCTVA-CPF-CNPJ-S 
ST25X6           MOVE FILIAL-CGCCPF-ST OF  OBJEB000
ST25X6                                 TO GFCTVA-FILIAL-S 
ST25X6           MOVE CONTR-CGCCPF-ST  OF  OBJEB000
LIE                                    TO GFCTVA-CONTROLE-S 
LIE        END-IF

LIE        IF    WRK-ACHOU10            EQUAL   'S'
ST25X6           MOVE POUPADOR-CGC-ST      OF  OBJEB010
ST25X6                                     TO GFCTVA-CPF-CNPJ-S 
ST25X6           MOVE POUPADOR-FILIAL-ST   OF  OBJEB010
ST25X6                                     TO GFCTVA-FILIAL-S 
ST25X6           MOVE POUPADOR-CONTR-ST    OF  OBJEB010
ST25X6                                     TO GFCTVA-CONTROLE-S 
LIE        END-IF

LIE        IF    WRK-ACHOU4             EQUAL   'S'
ST25X6           MOVE CCGC-CPF-ST       OF CLIEV004
ST25X6                                  TO GFCTVA-CPF-CNPJ-S 
ST25X6           MOVE CFLIAL-CGC-ST     OF CLIEV004
ST25X6                                  TO GFCTVA-FILIAL-S
ST25X6           MOVE CCTRL-CPF-CGC-ST  OF CLIEV004
ST25X6                                  TO WRK-CCTRL-CPF
ST25X6           MOVE WRK-CCTRL-CPF-AUX TO GFCTVA-CONTROLE-S 
           ELSE
LIE          IF  WRK-ACHOU4         EQUAL   'N'
ST25X6           MOVE  SPACES           TO   GFCTVA-CPF-CNPJ
ST25X6                                         GFCTVA-FILIAL
ST25X6                                         GFCTVA-CONTROLE
LIE          END-IF
LIE        END-IF

           IF  GFCTVA-DOCOR-EVNTO      NOT EQUAL WRK-DATA-ANT    OR
LIE            WRK-CSGMTO-CLI          NOT EQUAL WRK-CSGMTO-CLI-ANT
               PERFORM 3170-PESQUISA-COD-SEGMENTO
               MOVE GFCTVA-DOCOR-EVNTO TO WRK-DATA-ANT
LIE            MOVE WRK-CSGMTO-CLI     TO WRK-CSGMTO-CLI-ANT
           END-IF

           MOVE CSGMTO-GSTAO-TARIF     OF GFCTB0H3
                                       TO GFCTVA-CSGMTO-GSTAO-TARIF.
           PERFORM 3200-GRAVAR-EVENTOS.

           PERFORM 2100-LER-EVENTO92.

      *----------------------------------------------------------------*
       3000-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *    PESQUISA DE AGRUPAMENTOS.                                   *
      ******************************************************************
      *----------------------------------------------------------------*
       3100-PESQUISA-AGRUPAMENTOS      SECTION.
      *----------------------------------------------------------------*

           PERFORM 3110-ACESSA-GFCT5564.

LIE        MOVE  SPACES           TO     WRK-ACHOU0    WRK-ACHOU10
LIE                                      WRK-ACHOU4.

           PERFORM 3120-PESQUISA-CLIENTE.

           MOVE WRK-CH-MOVIMENTO-ATU    TO WRK-CH-MOVIMENTO-ANT.
           MOVE GFCTVA-CPAB-DSTNO-MOVTO TO WRK-PAB-ANT

           IF  GFCTVA-CPAB-DSTNO-MOVTO NOT EQUAL ZEROS
               PERFORM 3180-PESQUISA-BCO-POSTAL
           ELSE
               MOVE ZEROS          TO WRK-BCO-POSTAL
               MOVE ZEROS          TO WRK-CPAB
           END-IF.

      *----------------------------------------------------------------*
       3100-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *    OBTEM CODIGO DO MUNICIPIO E SIGLA DA UF DA AGENCIA PELO MO- *
      *    DULO GFCT5564.                                              *
      ******************************************************************
      *----------------------------------------------------------------*
       3110-ACESSA-GFCT5564            SECTION.
      *----------------------------------------------------------------*

           MOVE SPACES                 TO GFCTRK-AREA-RETORNO
                                          GFCT0M-ERRO-AREAS.

           INITIALIZE GFCTRK-AREA-RETORNO
                      GFCT0M-ERRO-AREAS.

           MOVE 'B'                    TO GFCTRK-TPO-AMBIENTE.
           MOVE GFCTVA-CAG-DSTNO-MOVTO TO GFCTRK-CJUNC-DEPDC.
           MOVE 'GFCT5564'             TO WRK-MODULO.

           CALL WRK-MODULO             USING GFCTRK-AREA
                                             GFCT0M-ERRO-AREAS.

           IF  RETURN-CODE             NOT EQUAL ZEROS
               DISPLAY '************** GFCT1892 **************'
               DISPLAY '*  ERRO NO ACESSO AO MODULO '
               DISPLAY '*  GFCT5564 PELA AGENCIA:   '
               DISPLAY '*  ' GFCTVA-CAG-DSTNO-MOVTO
               PERFORM 4000-FINALIZAR
           END-IF.

           IF  GFCTRK-COD-RETORNO      EQUAL 01
               DISPLAY '************** GFCT1892 **************'
               DISPLAY '*  ERRO NO ACESSO AO MODULO GFCT5564  '
               DISPLAY '*  TIPO AMBIENTE INVALIDO             '
               PERFORM 4000-FINALIZAR
           END-IF.

           IF  GFCTRK-COD-RETORNO      EQUAL 02
               DISPLAY '************** GFCT1892 **************'
               DISPLAY '*  ERRO NO ACESSO AO MODULO GFCT5564  '
               DISPLAY '*  COD AGENCIA NAO NUMERICO OU ZEROS  '
               MOVE ZEROS  TO GFCTRK-CMUN-RURAL
               MOVE SPACES TO GFCTRK-CSGL-UF
           END-IF.

           IF  GFCTRK-COD-RETORNO      EQUAL 99
               DISPLAY '************** GFCT1892 **************'
               DISPLAY '* ERRO DB2 ACESSO AO MODULO GFCT5564  '
               DISPLAY '* TABELA: ' GFCT0M-NOME-TAB '         '
               DISPLAY '*  ' GFCTVA-CAG-DSTNO-MOVTO
               MOVE ZEROS  TO GFCTRK-CMUN-RURAL
               MOVE SPACES TO GFCTRK-CSGL-UF
           END-IF.

           IF  GFCTRK-COD-RETORNO      EQUAL 03
               DISPLAY '************** GFCT1892 **************'
               DISPLAY '*  ERRO NO ACESSO AO MODULO GFCT5564  '
               DISPLAY '*  AGENCIA INEXISTENTE: ' GFCTVA-CAG-DSTNO-MOVTO
               GFCTVA-CAG-DSTNO-MOVTO
               MOVE ZEROS  TO GFCTRK-CMUN-RURAL
               MOVE SPACES TO GFCTRK-CSGL-UF
           END-IF.

      *----------------------------------------------------------------*
       3110-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *    OBTEM CODIGO DO SEGMENTO E CODIGO POSTO.                    *
      ******************************************************************
      *----------------------------------------------------------------*
       3120-PESQUISA-CLIENTE           SECTION.
      *----------------------------------------------------------------*

           IF  GFCTVA-CCTA-DSTNO-MOVTO > 999999
               PERFORM 3130-PESQUISA-CLIEV007
LIE            PERFORM 3131-PESQUISA-OBJEB010
           ELSE
               PERFORM 3140-PESQUISA-CLIEV008
LIE            PERFORM 3141-PESQUISA-OBJEB000
           END-IF

LIE        IF  WRK-ACHOU0             EQUAL  'N'    OR
LIE            WRK-ACHOU10            EQUAL  'N'
               PERFORM 3160-PESQUISA-CLIEV004
LIE        END-IF.

      *----------------------------------------------------------------*
       3120-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *    PESQUISA A TABELA CLIEV007.                                 *
      ******************************************************************
      *----------------------------------------------------------------*
       3130-PESQUISA-CLIEV007          SECTION.
      *----------------------------------------------------------------*

           MOVE GFCTVA-CAG-DSTNO-MOVTO  TO CJUNC-DEPDC     OF CLIEV007.
           MOVE GFCTVA-CCTA-DSTNO-MOVTO TO CCTA-CLI        OF CLIEV007.

           EXEC SQL
             SELECT
                   CID_CLI,
                   CPOSTO_SERVC,
                   CSGMTO_CLI,
                   CGRP_CTBIL_ATUAL,
                   CSGRP_CTBIL
             INTO
                   :CLIEV007.CID-CLI,
                   :CLIEV007.CPOSTO-SERVC,
                   :CLIEV007.CSGMTO-CLI,
                   :CLIEV007.CGRP-CTBIL-ATUAL,
                   :CLIEV007.CSGRP-CTBIL
             FROM   DB2PRD.V01CTA_POUPANCA
             WHERE
                   CJUNC_DEPDC         = :CLIEV007.CJUNC-DEPDC  AND
                   CCTA_CLI            = :CLIEV007.CCTA-CLI
           END-EXEC.

           IF  (SQLCODE                NOT EQUAL ZEROS AND +100) OR
               (SQLWARN0               EQUAL 'W')
                MOVE 'DB2'             TO ERR-TIPO-ACESSO
                MOVE 'V01CTA_POUPANCA' TO ERR-DBD-TAB
                MOVE 'SELECT'          TO ERR-FUN-COMANDO
                MOVE SQLCODE           TO ERR-SQL-CODE
                MOVE '0010'            TO ERR-LOCAL
                MOVE SPACES            TO ERR-SEGM
                PERFORM 9999-PROCESSAR-ROTINA-ERRO
           END-IF.

           IF  SQLCODE                 EQUAL ZEROS
               MOVE CID-CLI            OF CLIEV007
                                       TO WRK-CID-CLI
               MOVE CPOSTO-SERVC       OF CLIEV007
                                       TO WRK-COD-POSTO
               MOVE CSGMTO-CLI         OF CLIEV007
                                       TO WRK-CSGMTO-CLI
LFM            MOVE CGRP-CTBIL-ATUAL   OF CLIEV007
LFM                                    TO WRK-GRUPO
LFM            MOVE CSGRP-CTBIL        OF CLIEV007
                                       TO WRK-SUB-GRUPO
           END-IF.

LIE'       IF  SQLCODE             EQUAL +100
               MOVE  '3130' TO WRK-ROTINA
               PERFORM 3150-DISPLAY-MENSAGEM-ERRO
               MOVE  ZEROS TO WRK-CID-CLI
                                  WRK-COD-POSTO
                                  WRK-CSGMTO-CLI
                                  WRK-GRUPO
                                  WRK-SUB-GRUPO
LIE''      END-IF.

      *----------------------------------------------------------------*
       3130-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
LIE'  *    PESQUISA A TABELA OBJEB010 POUPANCA                         *
      ******************************************************************
      *----------------------------------------------------------------*
       3131-PESQUISA-OBJEB010          SECTION.
      *----------------------------------------------------------------*

           MOVE GFCTVA-CAG-DSTNO-MOVTO  TO POUPADOR-AGENCIA OF OBJEB010.
           MOVE GFCTVA-CCTA-DSTNO-MOVTO TO POUPADOR-CONTA  OF OBJEB010.

           EXEC SQL
             SELECT
                   POUPADOR_CGC_CPF,
                   POUPADOR_FILIAL,
                   POUPADOR_CONTROLE,
ST25X6             POUPADOR_CGC_CPF_ST,
ST25X6             POUPADOR_FILIAL_ST,
ST25X6             POUPADOR_CONTROLE_ST
             INTO
                   :OBJEB010.POUPADOR-CGC-CPF,
                   :OBJEB010.POUPADOR-FILIAL,
                   :OBJEB010.POUPADOR-CONTROLE,
ST25X6             :OBJEB010.POUPADOR-CGC-ST,
ST25X6             :OBJEB010.POUPADOR-FILIAL-ST,
ST25X6             :OBJEB010.POUPADOR-CONTR-ST
             FROM   DB2PRD.OBJE_CAD_POUPADOR
             WHERE
                   POUPADOR_AGENCIA    = :OBJEB010.POUPADOR-AGENCIA  AND
                   POUPADOR_CONTA      = :OBJEB010.POUPADOR-CONTA
           END-EXEC.

           IF  (SQLCODE                NOT EQUAL ZEROS AND +100) OR
               (SQLWARN0               EQUAL 'W')
                MOVE 'DB2'             TO ERR-TIPO-ACESSO
                MOVE 'OBJE_CAD_POUPADOR'
                                       TO ERR-DBD-TAB
                MOVE 'SELECT'          TO ERR-FUN-COMANDO
                MOVE SQLCODE           TO ERR-SQL-CODE
                MOVE '0031'            TO ERR-LOCAL
                MOVE SPACES            TO ERR-SEGM
                PERFORM 9999-PROCESSAR-ROTINA-ERRO
           END-IF.

           IF   SQLCODE                EQUAL  ZEROS
                MOVE 'S'               TO     WRK-ACHOU10
           ELSE
                MOVE 'N'               TO     WRK-ACHOU10
           END-IF.

LIE        ADD 1                       TO ACU-LIDOS-OBJEB010.

      *----------------------------------------------------------------*
LIE''  3131-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *    PESQUISA A TABELA CLIEV008.                                 *
      ******************************************************************
      *----------------------------------------------------------------*
       3140-PESQUISA-CLIEV008          SECTION.
      *----------------------------------------------------------------*

           MOVE GFCTVA-CAG-DSTNO-MOVTO  TO CJUNC-DEPDC     OF CLIEV008.
           MOVE GFCTVA-CCTA-DSTNO-MOVTO TO CCTA-CLI        OF CLIEV008.

           EXEC SQL
             SELECT
                   CID_CLI,
                   CPOSTO_SERVC,
                   CSGMTO_CLI,
LFM                CGRP_CTBIL_ATUAL,
LFM                CSGRP_CTBIL
             INTO
                   :CLIEV008.CID-CLI,
                   :CLIEV008.CPOSTO-SERVC,
                   :CLIEV008.CSGMTO-CLI,
LFM                :CLIEV008.CGRP-CTBIL-ATUAL,
LFM                :CLIEV008.CSGRP-CTBIL
             FROM   DB2PRD.V01CTA_CORRENTE
             WHERE
                   CJUNC_DEPDC         = :CLIEV008.CJUNC-DEPDC  AND
                   CCTA_CLI            = :CLIEV008.CCTA-CLI
           END-EXEC.

           IF  (SQLCODE                NOT EQUAL ZEROS AND +100) OR
               (SQLWARN0               EQUAL 'W')
                MOVE 'DB2'             TO ERR-TIPO-ACESSO
                MOVE 'V01CTA_CORRENTE' TO ERR-DBD-TAB
                MOVE 'SELECT'          TO ERR-FUN-COMANDO
                MOVE SQLCODE           TO ERR-SQL-CODE
                MOVE '0020'            TO ERR-LOCAL
                MOVE SPACES            TO ERR-SEGM
                PERFORM 9999-PROCESSAR-ROTINA-ERRO
           END-IF.

           IF  SQLCODE                 EQUAL ZEROS
               MOVE CID-CLI            OF CLIEV008
                                       TO WRK-CID-CLI
               MOVE CPOSTO-SERVC       OF CLIEV008
                                       TO WRK-COD-POSTO
               MOVE CSGMTO-CLI         OF CLIEV008
                                       TO WRK-CSGMTO-CLI
LFM            MOVE CGRP-CTBIL-ATUAL   OF CLIEV008
LFM                                    TO WRK-GRUPO
LFM            MOVE CSGRP-CTBIL        OF CLIEV008
LFM                                    TO WRK-SUB-GRUPO
           END-IF.

LIE'       IF  SQLCODE             EQUAL +100
               MOVE  '3140' TO WRK-ROTINA
               PERFORM 3150-DISPLAY-MENSAGEM-ERRO
               MOVE  ZEROS TO WRK-CID-CLI
                                  WRK-COD-POSTO
                                  WRK-CSGMTO-CLI
                                  WRK-GRUPO
                                  WRK-SUB-GRUPO
LIE''      END-IF.

      *----------------------------------------------------------------*
       3140-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
LIE'  *    PESQUISA A TABELA OBJEB000 CONTA CORRENTE                   *
      ******************************************************************
      *----------------------------------------------------------------*
       3141-PESQUISA-OBJEB000          SECTION.
      *----------------------------------------------------------------*

           MOVE GFCTVA-CAG-DSTNO-MOVTO  TO AGENCIA  OF OBJEB000.
           MOVE GFCTVA-CCTA-DSTNO-MOVTO TO CONTA-CORRENTE  OF OBJEB000.

           EXEC SQL
             SELECT
                   NUM_CGCCPF,
                   FILIAL_CGCCPF,
                   CONTR_CGCCPF,
ST25X6             NUM_CGCCPF_ST,
ST25X6             FILIAL_CGCCPF_ST,
ST25X6             CONTR_CGCCPF_ST
             INTO
                   :OBJEB000.NUM-CGCCPF,
                   :OBJEB000.FILIAL-CGCCPF,
                   :OBJEB000.CONTR-CGCCPF
ST25X6             :OBJEB000.NUM-CGCCPF-ST,
ST25X6             :OBJEB000.FILIAL-CGCCPF-ST,
ST25X6             :OBJEB000.CONTR-CGCCPF-ST
             FROM   DB2PRD.OBJE_CLIENTES
             WHERE
                   AGENCIA             = :OBJEB000.AGENCIA    AND
                   CONTA_CORRENTE      = :OBJEB000.CONTA-CORRENTE
           END-EXEC.

           IF  (SQLCODE                NOT EQUAL ZEROS AND +100) OR
               (SQLWARN0               EQUAL 'W')
                MOVE 'DB2'             TO ERR-TIPO-ACESSO
                MOVE 'OBJE_CLIENTES'
                                       TO ERR-DBD-TAB
                MOVE 'SELECT'          TO ERR-FUN-COMANDO
                MOVE SQLCODE           TO ERR-SQL-CODE
                MOVE '0041'            TO ERR-LOCAL
                MOVE SPACES            TO ERR-SEGM
                PERFORM 9999-PROCESSAR-ROTINA-ERRO
           END-IF.

           IF   SQLCODE                EQUAL  ZEROS
                MOVE 'S'               TO     WRK-ACHOU0
           ELSE
                MOVE 'N'               TO     WRK-ACHOU0
           END-IF.

           ADD 1                       TO ACU-LIDOS-OBJEB000.

      *----------------------------------------------------------------*
LIE''  3141-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *    DISPLAY DE MENSAGEM DE ERRO.                                *
      ******************************************************************
      *----------------------------------------------------------------*
       3150-DISPLAY-MENSAGEM-ERRO      SECTION.
      *----------------------------------------------------------------*

           DISPLAY '************** GFCT1892 **************************'.
           DISPLAY '*  ERRO NO ACESSO AO CLIE - CLIENTE NAO ENCONTRADO'
LIE        DISPLAY '*  ROTINA = ' WRK-ROTINA
LIE        DISPLAY '*  PELA AGENCIA/CONTA: ' GFCTVA-CAG-DSTNO-MOVTO
                   ' / ' GFCTVA-CCTA-DSTNO-MOVTO.

      *----------------------------------------------------------------*
       3150-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *    PESQUISA A TABELA CLIEV004.                                 *
      ******************************************************************
      *----------------------------------------------------------------*
       3160-PESQUISA-CLIEV004          SECTION.
      *----------------------------------------------------------------*

           MOVE WRK-CID-CLI            TO CID-CLI OF CLIEV004.

           EXEC SQL
             SELECT
                   CCGC_CPF,
                   CFLIAL_CGC,
                   CCTRL_CPF_CGC,
ST25X6             CCGC_CPF_ST,
ST25X6             CFLIAL_CGC_ST,
ST25X6             CCTRL_CPF_CGC_ST
             INTO
                   :CLIEV004.CCGC-CPF,
                   :CLIEV004.CFLIAL-CGC,
                   :CLIEV004.CCTRL-CPF-CGC,
ST25X6             :CLIEV004.CCGC-CPF-ST,
ST25X6             :CLIEV004.CFLIAL-CGC-ST,
ST25X6             :CLIEV004.CCTRL-CPF-CGC-ST
             FROM   DB2PRD.V01CLIENTE_AGENCIA
             WHERE
                   CID_CLI             = :CLIEV004.CID-CLI
           END-EXEC.

           IF  (SQLCODE                NOT EQUAL ZEROS AND +100) OR
               (SQLWARN0               EQUAL 'W')
                MOVE 'DB2'             TO ERR-TIPO-ACESSO
                MOVE 'V01CLIENTE_AGENCIA'
                                       TO ERR-DBD-TAB
                MOVE 'SELECT'          TO ERR-FUN-COMANDO
                MOVE SQLCODE           TO ERR-SQL-CODE
                MOVE '0030'            TO ERR-LOCAL
                MOVE SPACES            TO ERR-SEGM
                PERFORM 9999-PROCESSAR-ROTINA-ERRO
           END-IF.

LIE        IF      SQLCODE            EQUAL +100
LIE                MOVE  '3160' TO WRK-ROTINA
                   PERFORM 3150-DISPLAY-MENSAGEM-ERRO
                   MOVE  ZEROS  TO WRK-CSGMTO-CLI
                                   WRK-COD-POSTO
                   MOVE  SPACES  TO CFLIAL-CGC-ST    OF CLIEV004
                                    CCGC-CPF-ST      OF CLIEV004
                   MOVE  ZEROS  TO CCTRL-CPF-CGC-ST  OF CLIEV004
LIE                MOVE 'N'     TO WRK-ACHOU4
LIE        ELSE
LIE                MOVE 'S'     TO WRK-ACHOU4
LIE        END-IF.

LIE        ADD 1                       TO ACU-LIDOS-CLIEV004.

      *----------------------------------------------------------------*
       3160-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *    PESQUISA CODIGO SEGMENTO DA TABELA GFCTB0H3 (ESPECIFICO)    *
      ******************************************************************
      *----------------------------------------------------------------*
       3170-PESQUISA-COD-SEGMENTO      SECTION.
      *----------------------------------------------------------------*

           MOVE WRK-CSGMTO-CLI         TO CSGMTO-INIC-CLI   OF GFCTB0H3.

           MOVE GFCTVA-DOCOR-EVNTO     TO DINIC-VGCIA-ASSOC OF GFCTB0H3
                                          DFIM-VGCIA-ASSOC  OF GFCTB0H3.

           EXEC SQL
             SELECT
                   CSGMTO_GSTAO_TARIF
             INTO
                   :GFCTB0H3.CSGMTO-GSTAO-TARIF
             FROM   DB2PRD.TASSOC_SGMTO_CLI
             WHERE
                   DINIC_VGCIA_ASSOC <= :GFCTB0H3.DINIC-VGCIA-ASSOC AND
                   DFIM_VGCIA_ASSOC  >= :GFCTB0H3.DFIM-VGCIA-ASSOC  AND
                   CSGMTO_INIC_CLI    = :GFCTB0H3.CSGMTO-INIC-CLI   AND
                   CINDCD_INTVL_ESPEC = 2
           END-EXEC.

           IF  (SQLCODE                NOT EQUAL ZEROS AND +100) OR
               (SQLWARN0               EQUAL 'W')
                MOVE 'DB2'             TO ERR-TIPO-ACESSO
                MOVE 'TASSOC_SGMTO_CLI'
                                       TO ERR-DBD-TAB
                MOVE 'SELECT'          TO ERR-FUN-COMANDO
                MOVE SQLCODE           TO ERR-SQL-CODE
                MOVE '0040'            TO ERR-LOCAL
                MOVE SPACES            TO ERR-SEGM
                PERFORM 9999-PROCESSAR-ROTINA-ERRO
           END-IF.

           IF  SQLCODE                 EQUAL +100
               PERFORM 3175-PESQ-COD-SEGM-INTERV
               IF  SQLCODE                 EQUAL +100
                   MOVE ZEROS              TO CSGMTO-GSTAO-TARIF
               END-IF
           END-IF.

      *----------------------------------------------------------------*
       3170-99-FIM.                    EXIT.
      *----------------------------------------------------------------*
      ******************************************************************
      *    PESQUISA CODIGO SEGMENTO DA TABELA GFCTB0H3 (INTERVALO)     *
      ******************************************************************
      *----------------------------------------------------------------*
       3175-PESQ-COD-SEGM-INTERV       SECTION.
      *----------------------------------------------------------------*

           MOVE WRK-CSGMTO-CLI         TO CSGMTO-INIC-CLI   OF GFCTB0H3
                                          CSGMTO-FNAL-CLI   OF GFCTB0H3.

           MOVE GFCTVA-DOCOR-EVNTO     TO DINIC-VGCIA-ASSOC OF GFCTB0H3
                                          DFIM-VGCIA-ASSOC  OF GFCTB0H3.

           EXEC SQL
             SELECT
                   CSGMTO_GSTAO_TARIF
             INTO
                   :GFCTB0H3.CSGMTO-GSTAO-TARIF
             FROM   DB2PRD.TASSOC_SGMTO_CLI
             WHERE
                   DINIC_VGCIA_ASSOC <= :GFCTB0H3.DINIC-VGCIA-ASSOC AND
                   DFIM_VGCIA_ASSOC  >= :GFCTB0H3.DFIM-VGCIA-ASSOC  AND
                   CSGMTO_INIC_CLI   <= :GFCTB0H3.CSGMTO-INIC-CLI   AND
                   CSGMTO_FNAL_CLI   >= :GFCTB0H3.CSGMTO-FNAL-CLI   AND
                   CINDCD_INTVL_ESPEC = 1
           END-EXEC.

           IF  (SQLCODE                NOT EQUAL ZEROS AND +100) OR
               (SQLWARN0               EQUAL 'W')
                MOVE 'DB2'             TO ERR-TIPO-ACESSO
                MOVE 'TASSOC_SGMTO_CLI'
                                       TO ERR-DBD-TAB
                MOVE 'SELECT'          TO ERR-FUN-COMANDO
                MOVE SQLCODE           TO ERR-SQL-CODE
                MOVE '0042'            TO ERR-LOCAL
                MOVE SPACES            TO ERR-SEGM
                PERFORM 9999-PROCESSAR-ROTINA-ERRO
           END-IF.


      *----------------------------------------------------------------*
       3175-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *    CHAMA MODULO MESU9718 PARA VER SE EH POSTO OU BCO POSTAL.   *
      ******************************************************************
      *----------------------------------------------------------------*
       3180-PESQUISA-BCO-POSTAL        SECTION.
      *----------------------------------------------------------------*

           MOVE 'VRS001'               TO COMU-VERSAO.
           MOVE 237                    TO COMU-COD-EMPR.
           MOVE GFCTVA-CAG-DSTNO-MOVTO
                                       TO COMU-COD-DEPDC.
           MOVE GFCTVA-CPAB-DSTNO-MOVTO
                                       TO COMU-COD-POSTO.
           MOVE SPACES                 TO COMU-CORR-BANCARIO.
           MOVE 'MESU9718'             TO WRK-MODULO.

           CALL WRK-MODULO             USING  COMU-DADOS-9018.


           IF RETURN-CODE             NOT EQUAL ZEROS
              IF RETURN-CODE         EQUAL 4
                 DISPLAY '************** GFCT1892 **************'
                 DISPLAY '*  ERRO NO ACESSO AO MODULO MESU9718  '
                 DISPLAY '*  ' COMU-MENSAGEM
                 DISPLAY '*  AGENCIA = ' GFCTVA-CAG-DSTNO-MOVTO
                         ' / PAB     = ' GFCTVA-CPAB-DSTNO-MOVTO
                 MOVE ZEROS  TO WRK-BCO-POSTAL
                 MOVE ZEROS  TO WRK-CPAB
              ELSE
                 IF RETURN-CODE     EQUAL 8
                    DISPLAY '************** GFCT1892 **************'
                    DISPLAY '*  ERRO NO ACESSO AO MODULO MESU9718  '
                    DISPLAY '*  ' COMU-MENSAGEM
                    DISPLAY '*  AGENCIA = ' GFCTVA-CAG-DSTNO-MOVTO
                            ' / PAB     = ' GFCTVA-CPAB-DSTNO-MOVTO
                    MOVE ZEROS  TO WRK-BCO-POSTAL
                    MOVE ZEROS  TO WRK-CPAB
                 ELSE
                    IF RETURN-CODE EQUAL 12
                       DISPLAY '************** GFCT1892 **************'
                       DISPLAY '*  ERRO NO ACESSO AO MODULO MESU9718  '
                       DISPLAY '*  ' COMU-MENSAGEM
                       DISPLAY '*  AGENCIA = ' GFCTVA-CAG-DSTNO-MOVTO
                               ' / PAB     = ' GFCTVA-CPAB-DSTNO-MOVTO
                       MOVE ZEROS  TO WRK-BCO-POSTAL
                       MOVE ZEROS  TO WRK-CPAB
                    ELSE
                       DISPLAY '************** GFCT1892 **************'
                       DISPLAY '*  ERRO NO ACESSO AO MODULO MESU9718  '
                       DISPLAY '*  DIFERENTE DE 04 OU 08 OU 12        '
                       DISPLAY '*  ' COMU-MENSAGEM
                       DISPLAY '*  AGENCIA = ' GFCTVA-CAG-DSTNO-MOVTO
                               ' / PAB     = ' GFCTVA-CPAB-DSTNO-MOVTO
                       MOVE ZEROS  TO WRK-BCO-POSTAL
                       MOVE ZEROS  TO WRK-CPAB
                    END-IF
                 END-IF
              END-IF
              DISPLAY '************** GFCT1892 **************'
              DISPLAY '*  ERRO NO ACESSO AO MODULO MESU9718  '
              DISPLAY '*  AGENCIA: ' GFCTVA-CAG-DSTNO-MOVTO
                      ' / PAB: ' GFCTVA-CPAB-DSTNO-MOVTO
              MOVE ZEROS  TO WRK-BCO-POSTAL
              MOVE ZEROS  TO WRK-CPAB
           ELSE
               IF  COMU-COD-TIPO-POSTO NOT EQUAL 9
                   MOVE GFCTVA-CPAB-DSTNO-MOVTO TO WRK-CPAB
                   MOVE ZEROS  TO WRK-BCO-POSTAL
               ELSE
                   MOVE GFCTVA-CPAB-DSTNO-MOVTO TO WRK-BCO-POSTAL
                   MOVE ZEROS  TO WRK-CPAB
               END-IF
           END-IF.

      *----------------------------------------------------------------*
       3180-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *    GRAVAR EVENTOS.                                             *
      ******************************************************************
      *----------------------------------------------------------------*
       3200-GRAVAR-EVENTOS             SECTION.
      *----------------------------------------------------------------*

           MOVE WRK-GRAVACAO           TO WRK-OPERACAO.

           WRITE REG-EVENTOAT          FROM GFCTVA-GFCTB092.

           PERFORM 1120-TESTAR-FS-EVENTOAT.

           ADD 1                       TO ACU-GRAVS-EVENTOAT.

      *----------------------------------------------------------------*
       3200-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *    ROTINA DE FINALIZACAO. FECHAR ARQUIVOS E TESTAR FILE STATUS *
      ******************************************************************
      *----------------------------------------------------------------*
       4000-FINALIZAR                  SECTION.
      *----------------------------------------------------------------*

           IF  ACU-GRAVS-EVENTOAT      NOT EQUAL ZEROS
               PERFORM 4100-EMITIR-DISPLAY-SUCESSO
           END-IF.

           CLOSE EVENTO92
                 EVENTOAT.

           MOVE WRK-FECHAMENTO         TO WRK-OPERACAO.

           PERFORM 1100-TESTAR-FILE-STATUS.

           STOP RUN.

      *----------------------------------------------------------------*
       4000-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *    EMITE DISPLAY COM TOTAL DE LIDOS E GRAVADOS.                *
      ******************************************************************
      *----------------------------------------------------------------*
       4100-EMITIR-DISPLAY-SUCESSO     SECTION.
      *----------------------------------------------------------------*

           DISPLAY '*------------------------------------------------*'.
           DISPLAY '*               PROGRAMA GFCT1892                *'.
           DISPLAY '*               -----------------                *'.
           DISPLAY '*                                                *'.
           DISPLAY '*       PROCESSAMENTO EFETUADO COM SUCESSO       *'.
           DISPLAY '*------------------------------------------------*'.

           DISPLAY '****************************************************
      -            '**********'.
           MOVE ACU-LIDOS-EVENTO92     TO WRK-MASCARA.
           DISPLAY '* TOTAL DE REGISTROS LIDOS    NO EVENTOS      = '
                    WRK-MASCARA ' *'.
LIE        MOVE ACU-LIDOS-OBJEB000     TO WRK-MASCARA.
           DISPLAY '* TOTAL DE REGISTROS LIDOS    NA OBJEB000     = '
                    WRK-MASCARA ' *'.
LIE        MOVE ACU-LIDOS-OBJEB010     TO WRK-MASCARA.
           DISPLAY '* TOTAL DE REGISTROS LIDOS    NA OBJEB010     = '
                    WRK-MASCARA ' *'.
LIE        MOVE ACU-LIDOS-CLIEV004     TO WRK-MASCARA.
           DISPLAY '* TOTAL DE REGISTROS LIDOS    NA CLIEV004     = '
                    WRK-MASCARA ' *'.
           MOVE ACU-GRAVS-EVENTOAT     TO WRK-MASCARA.
           DISPLAY '* TOTAL DE REGISTROS GRAVADOS EM EVENTOS-ATU  = '
                    WRK-MASCARA ' *'.
           DISPLAY '****************************************************
      -            '**********'.

      *----------------------------------------------------------------*
       4100-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *    TRATAMENTO DE ERROS.                                        *
      ******************************************************************
      *----------------------------------------------------------------*
       9999-PROCESSAR-ROTINA-ERRO      SECTION.
      *----------------------------------------------------------------*

           MOVE 'GFCT1892'             TO ERR-PGM.

           CALL 'POOL7100'             USING WRK-BATCH
                                             ERRO-AREA
                                             SQLCA.

           GOBACK.

      *----------------------------------------------------------------*
       9999-99-FIM.                    EXIT.
      *----------------------------------------------------------------*