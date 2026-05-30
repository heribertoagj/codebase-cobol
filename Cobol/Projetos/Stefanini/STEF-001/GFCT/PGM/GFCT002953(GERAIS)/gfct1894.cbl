      *================================================================*
       IDENTIFICATION                  DIVISION.
      *================================================================*

       PROGRAM-ID. GFCT1894.
       AUTHOR.     ROSANETE GRASSIANI DOS SANTOS.
      *================================================================*
      *                    C P M - S I S T E M A S                     *
      *----------------------------------------------------------------*
      *    PROGRAMA....:   GFCT1894                                    *
      *    PROGRAMADORA:   ROSANETE GRASSIANI S.   - CPM PATO BRANCO   *
      *    ANALISTA CPM:   KHARUZO INOCENCIO LEITE - CPM PATO BRANCO   *
      *    ANALISTA....:   MARIA VALERIA TORQUATO  - PROCKWORK/ GP. 50 *
      *    DATA........:   06/11/2006                                  *
      *----------------------------------------------------------------*
      *    OBJETIVO....:   VERIFICAR SE TEM SUSPENSAO PARA A TARIFA  / *
      *      AGRUPAMENTO DE CPF/CNPJ.                                  *
      *----------------------------------------------------------------*
      *    BCO DE DADOS:                                               *
      *                DB2                                             *
      *                  TABLE                       INCLUDE/BOOK      *
      *                    DB2PRD.SUSP_GRP_CLI         GFCTB0E3        *
      *                    DB2PRD.SUSP_TARIF_AGPTO     GFCTB0E7        *
      *----------------------------------------------------------------*
      *    ARQUIVOS....:                                               *
      *                DDNAME                        INCLUDE/BOOK      *
      *                  EVENTO92                      I#GFCTVA        *
      *                  EVENTOAT                      I#GFCTVA        *
      *                  SUSPENSO                      I#GFCTVA        *
      *----------------------------------------------------------------*
      *    BOOK'S......:                                               *
      *    POL7100C - AREA PARA TRATAMENTO DE ERRO PELA POOL7100       *
      *----------------------------------------------------------------*
      *    MODULOS.....:                                               *
      *    POOL1050 - CONEXAO DB2                                      *
      *    POOL7100 - MODULO PARA CANCELAMENTO DO PROGRAMA             *
      *================================================================*
      *================================================================*
      *    ULTIMA ALTERACAO: MAR/2007 - VALERIA                        *
      *    ATUALIZACAO DA INC I#GFCTVA                                 *
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

           SELECT SUSPENSO ASSIGN      TO UT-S-SUSPENSO
           FILE STATUS                 IS WRK-FS-SUSPENSO.

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

       01  FD-EVENTO92                 PIC  X(250).

      *----------------------------------------------------------------*
      *   OUTPUT:  REGISTROS EVENTOS ATUALIZADOS GFCTB092              *
      *            ORG. SEQUENCIAL     -   LRECL   =   250             *
      *----------------------------------------------------------------*

       FD  EVENTOAT
           RECORDING MODE IS F
           LABEL RECORD IS STANDARD
           BLOCK CONTAINS 0 RECORDS.

       01  FD-EVENTOAT                 PIC  X(250).

      *----------------------------------------------------------------*
      *    OUTPUT: REGISTROS EVENTOS SUSPENSOS                         *
      *            ORG. SEQUENCIAL     - LRECL = 250                   *
      *----------------------------------------------------------------*

       FD  SUSPENSO
           RECORDING MODE IS F
           LABEL RECORD IS STANDARD
           BLOCK CONTAINS 0 RECORDS.

       01  FD-SUSPENSO                 PIC  X(250).

      *----------------------------------------------------------------*
       WORKING-STORAGE                 SECTION.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       77  FILLER                      PIC  X(050)         VALUE
           'INICIO DA AREA DE WORKING'.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       77  FILLER                      PIC  X(050)         VALUE
           'AREA PARA ACUMULADORES'.
      *----------------------------------------------------------------*

       77  ACU-LIDOS-EVENTO92          PIC  9(011) COMP-3  VALUE ZEROS.
       77  ACU-GRAVADOS-EVENTOAT       PIC  9(011) COMP-3  VALUE ZEROS.
       77  ACU-SUSPENSOS-EVENTOAT      PIC  9(011) COMP-3  VALUE ZEROS.
       77  ACU-SUSPENSOS-EVENTO92      PIC  9(011) COMP-3  VALUE ZEROS.
       77  ACU-GRAVADOS-SUSPENSO       PIC  9(011) COMP-3  VALUE ZEROS.
       77  ACU-AGENDADOS-EVENTOAT      PIC  9(011) COMP-3  VALUE ZEROS.

      *----------------------------------------------------------------*
       77  FILLER                      PIC  X(050)         VALUE
           'AREA PARA VARIAVEIS AUXILIARES'.
      *----------------------------------------------------------------*

       77  WRK-BATCH                   PIC  X(008)         VALUE
           'BATCH'.

       77  WRK-MASCARA                 PIC  Z.ZZZ.ZZZ.999  VALUE ZEROS.
       77  WRK-FIM                     PIC  X(001)         VALUE SPACES.
       77  WRK-ACHOU-SUSPENSAO         PIC  X(001)         VALUE SPACES.

       01  WRK-CONTROLE-AUX            PIC  X(002)         VALUE SPACES.
       01  WRK-CONTROLE-AUX-R          REDEFINES WRK-CONTROLE-AUX.
           05  WRK-CONTROLE-AUX-REDEF  PIC  9(002).

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           'AREA PARA VARIAVEIS DE DATA'.
      *----------------------------------------------------------------*

       01  WRK-DATA-TABELA             PIC  X(010)         VALUE SPACES.

       01  WRK-DOCOR-EVNTO-AUX         PIC  9(008)         VALUE ZEROS.
       01  WRK-DOCOR-EVNTO-AUX-R       REDEFINES WRK-DOCOR-EVNTO-AUX.
           05  WRK-ANO-EVNTO-AUX       PIC  9(004).
           05  WRK-MES-EVNTO-AUX       PIC  9(002).
           05  WRK-DIA-EVNTO-AUX       PIC  9(002).

       01  WRK-DINIC-INV               PIC  9(008)         VALUE ZEROS.
       01  WRK-DINIC-INV-R             REDEFINES WRK-DINIC-INV.
           05  WRK-ANO-DINIC-INV-R     PIC  9(004).
           05  WRK-MES-DINIC-INV-R     PIC  9(002).
           05  WRK-DIA-DINIC-INV-R     PIC  9(002).

       01  WRK-DINIC-AUX               PIC  X(010)         VALUE SPACES.
       01  WRK-DINIC-AUX-R             REDEFINES WRK-DINIC-AUX.
           05  WRK-DIA-DINIC-AUX-R     PIC  9(002).
           05  FILLER                  PIC  X(001).
           05  WRK-MES-DINIC-AUX-R     PIC  9(002).
           05  FILLER                  PIC  X(001).
           05  WRK-ANO-DINIC-AUX-R     PIC  9(004).

       01  WRK-DFIM-INV                PIC  9(008)         VALUE ZEROS.
       01  WRK-DFIM-INV-R              REDEFINES WRK-DFIM-INV.
           05  WRK-ANO-DFIM-INV-R      PIC  9(004).
           05  WRK-MES-DFIM-INV-R      PIC  9(002).
           05  WRK-DIA-DFIM-INV-R      PIC  9(002).

       01  WRK-DFIM-AUX                PIC  X(010)         VALUE SPACES.
       01  WRK-DFIM-AUX-R              REDEFINES WRK-DFIM-AUX.
           05  WRK-DIA-DFIM-AUX-R      PIC  9(002).
           05  FILLER                  PIC  X(001).
           05  WRK-MES-DFIM-AUX-R      PIC  9(002).
           05  FILLER                  PIC  X(001).
           05  WRK-ANO-DFIM-AUX-R      PIC  9(004).

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           'AREA PARA CHAVES'.
      *----------------------------------------------------------------*

       01  WRK-CH-MOVEVNTO.
           05  WRK-CHAVE-01.
               10  WRK-CSERVC-TARIF    PIC  9(005)         VALUE ZEROS.
               10  WRK-CPF-CNPJ        PIC  9(009)         VALUE ZEROS.
               10  WRK-FILIAL          PIC  9(005)         VALUE ZEROS.
               10  WRK-CONTROLE        PIC  X(002)         VALUE SPACES.
           05  WRK-DOCOR-EVNTO.
               10  WRK-DIA-EVNTO       PIC  9(002)         VALUE ZEROS.
               10  FILLER              PIC  X(001)         VALUE SPACES.
               10  WRK-MES-EVNTO       PIC  9(002)         VALUE ZEROS.
               10  FILLER              PIC  X(001)         VALUE SPACES.
               10  WRK-ANO-EVNTO       PIC  9(004)         VALUE ZEROS.

       01  WRK-CH-ANT.
           05  WRK-CHAVE-ANT.
               10  WRK-CSERVC-ANT      PIC  9(005)         VALUE ZEROS.
               10  WRK-CPF-CNPJ-ANT    PIC  9(009)         VALUE ZEROS.
               10  WRK-FILIAL-ANT      PIC  9(005)         VALUE ZEROS.
               10  WRK-CONTROLE-ANT    PIC  X(002)         VALUE SPACES.
           05  WRK-DOCOR-MOVTO.
               10  WRK-DOCOR-ANT       PIC  X(010)         VALUE SPACES.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           'AREA PARA TESTES DE FILE-STATUS'.
      *----------------------------------------------------------------*

       01  WRK-FS-EVENTO92             PIC  X(002)         VALUE SPACES.
       01  WRK-FS-EVENTOAT             PIC  X(002)         VALUE SPACES.
       01  WRK-FS-SUSPENSO             PIC  X(002)         VALUE SPACES.

       01  WRK-ABERTURA                PIC  X(013)         VALUE
           ' NA ABERTURA '.
       01  WRK-LEITURA                 PIC  X(013)         VALUE
           ' NA LEITURA  '.
       01  WRK-GRAVACAO                PIC  X(013)         VALUE
           ' NA GRAVACAO '.
       01  WRK-FECHAMENTO              PIC  X(013)         VALUE
           'NO FECHAMENTO'.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           'MENSAGEM DE ERRO DE FILE-STATUS'.
      *----------------------------------------------------------------*

       01  WRK-ERRO-POOL7100.
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
           'REGISTRO DE EVENTOS'.
      *----------------------------------------------------------------*

       COPY I#GFCTVA.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           'AREA PARA POOL7100'.
      *----------------------------------------------------------------*

       COPY POL7100C.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           'AREA DE DB2'.
      *----------------------------------------------------------------*

           EXEC SQL
               INCLUDE SQLCA
           END-EXEC.

           EXEC SQL
               INCLUDE GFCTB0E3
           END-EXEC.

           EXEC SQL
               INCLUDE GFCTB0E7
           END-EXEC.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           'FIM DA AREA DE WORKING'.
      *----------------------------------------------------------------*

      *================================================================*
       PROCEDURE                       DIVISION.
      *================================================================*

      ******************************************************************
      * DENINIR ROTINAS DO PROGRAMA.                                   *
      ******************************************************************
      *----------------------------------------------------------------*
       0000-INICIAR                    SECTION.
      *----------------------------------------------------------------*

           PERFORM 1000-INICIALIZAR.

           PERFORM 2000-LER-EVENTO92.

           PERFORM 3000-PROCESSAR      UNTIL
                   WRK-FIM             EQUAL 'F'.

           PERFORM 4000-FINALIZAR.

      *----------------------------------------------------------------*
       0000-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      * INICIALIZAR PROGRAMA.                                          *
      ******************************************************************
      *----------------------------------------------------------------*
       1000-INICIALIZAR                SECTION.
      *----------------------------------------------------------------*

           CALL 'POOL1050'.

           OPEN  INPUT EVENTO92
                OUTPUT EVENTOAT
                       SUSPENSO.

           MOVE WRK-ABERTURA           TO WRK-OPERACAO.

           PERFORM 1100-TESTAR-FILE-STATUS.

           INITIALIZE WRK-CH-MOVEVNTO.

      *----------------------------------------------------------------*
       1000-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      * TESTAR FILE STATUS DOS ARQUIVOS DE ENTRADA E SAIDA.            *
      ******************************************************************
      *----------------------------------------------------------------*
       1100-TESTAR-FILE-STATUS         SECTION.
      *----------------------------------------------------------------*

           PERFORM 1110-TESTAR-FS-EVENTO92.

           PERFORM 1120-TESTAR-FS-EVENTOAT.

           PERFORM 1130-TESTAR-FS-SUSPENSO.

      *----------------------------------------------------------------*
       1100-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      * TESTE DE FILE STATUS DO ARQUIVO DE ENTRADA - EVENTO92.         *
      ******************************************************************
      *----------------------------------------------------------------*
       1110-TESTAR-FS-EVENTO92         SECTION.
      *----------------------------------------------------------------*

           IF  WRK-FS-EVENTO92         NOT EQUAL '00'
               MOVE 'EVENTO92'         TO WRK-NOME-ARQ
               MOVE WRK-FS-EVENTO92    TO WRK-FILE-STATUS
               MOVE WRK-ERRO-POOL7100  TO ERR-TEXTO
               MOVE 'APL'              TO ERR-TIPO-ACESSO
               PERFORM 9999-PROCESSAR-ROTINA-ERRO
           END-IF.

      *----------------------------------------------------------------*
       1110-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      * TESTE DE FILE STATUS DO ARQUIVO DE SAIDA - EVENTOAT.           *
      ******************************************************************
      *----------------------------------------------------------------*
       1120-TESTAR-FS-EVENTOAT         SECTION.
      *----------------------------------------------------------------*

           IF  WRK-FS-EVENTOAT         NOT EQUAL '00'
               MOVE 'EVENTOAT'         TO WRK-NOME-ARQ
               MOVE WRK-FS-EVENTOAT    TO WRK-FILE-STATUS
               MOVE WRK-ERRO-POOL7100  TO ERR-TEXTO
               MOVE 'APL'              TO ERR-TIPO-ACESSO
               PERFORM 9999-PROCESSAR-ROTINA-ERRO
           END-IF.

      *----------------------------------------------------------------*
       1120-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *    ROTINA DE TESTE DE FILE STATUS DO ARQUIVO SUSPENSO          *
      ******************************************************************
      *----------------------------------------------------------------*
       1130-TESTAR-FS-SUSPENSO         SECTION.
      *----------------------------------------------------------------*

           IF  WRK-FS-SUSPENSO         NOT EQUAL '00'
               MOVE 'APL'              TO ERR-TIPO-ACESSO
               MOVE 'SUSPENSO'         TO WRK-NOME-ARQ
               MOVE WRK-FS-SUSPENSO    TO WRK-FILE-STATUS
               MOVE WRK-ERRO-POOL7100  TO ERR-TEXTO
               PERFORM 9999-PROCESSAR-ROTINA-ERRO
           END-IF.

      *----------------------------------------------------------------*
       1130-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      * LER ARQUIVO DE ENTRADA - EVENTO92.                             *
      ******************************************************************
      *----------------------------------------------------------------*
       2000-LER-EVENTO92               SECTION.
      *----------------------------------------------------------------*

           READ EVENTO92               INTO GFCTVA-GFCTB092.

           IF  WRK-FS-EVENTO92         EQUAL '10'
               MOVE 'F'                TO WRK-FIM
               MOVE HIGH-VALUES        TO WRK-CH-MOVEVNTO
               GO TO 2000-99-FIM
           END-IF.

           MOVE WRK-LEITURA            TO WRK-OPERACAO.

           PERFORM 1110-TESTAR-FS-EVENTO92.

           ADD 1                       TO ACU-LIDOS-EVENTO92.

           MOVE GFCTVA-CPF-CNPJ        TO WRK-CPF-CNPJ.
           MOVE GFCTVA-FILIAL          TO WRK-FILIAL.
           MOVE GFCTVA-CONTROLE        TO WRK-CONTROLE.
           MOVE GFCTVA-DOCOR-EVNTO     TO WRK-DOCOR-EVNTO.
           MOVE GFCTVA-CSERVC-TARIF    TO WRK-CSERVC-TARIF.

      *----------------------------------------------------------------*
       2000-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      * PROCESSAMENTO PRINCIPAL DO PROGRAMA.                           *
      ******************************************************************
      *----------------------------------------------------------------*
       3000-PROCESSAR                  SECTION.
      *----------------------------------------------------------------*

           IF  WRK-CH-MOVEVNTO         NOT EQUAL WRK-CH-ANT
               PERFORM 3100-PESQUISAR-SUSPENSAO
           END-IF.

           IF  WRK-ACHOU-SUSPENSAO     EQUAL 'S'
               MOVE 15                 TO GFCTVA-CSIT-EVNTO-RECBD
               ADD 1                   TO ACU-SUSPENSOS-EVENTOAT
               PERFORM 3300-GRAVAR-SUSPENSO
           ELSE
               MOVE 02                 TO GFCTVA-CSIT-EVNTO-RECBD
               ADD 1                   TO ACU-AGENDADOS-EVENTOAT
               PERFORM 3200-GRAVAR-EVENTOAT
           END-IF

           PERFORM 2000-LER-EVENTO92.

      *----------------------------------------------------------------*
       3000-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      * EFETUA PESQUISA SUSPENSAO.                                     *
      ******************************************************************
      *----------------------------------------------------------------*
       3100-PESQUISAR-SUSPENSAO        SECTION.
      *----------------------------------------------------------------*

           MOVE 04                     TO CAGPTO-CTA       OF GFCTB0E7.
           MOVE GFCTVA-CSERVC-TARIF    TO CSERVC-TARIF     OF GFCTB0E7.
           MOVE GFCTVA-CPF-CNPJ        TO CCGC-CPF         OF GFCTB0E3.
           MOVE GFCTVA-FILIAL          TO CFLIAL-CGC       OF GFCTB0E3.
           MOVE GFCTVA-CONTROLE        TO WRK-CONTROLE-AUX.
           MOVE WRK-CONTROLE-AUX-REDEF TO CCTRL-CPF-CGC    OF GFCTB0E3.
           MOVE GFCTVA-DOCOR-EVNTO     TO DINIC-SUSP-COBR  OF GFCTB0E7
                                          DFIM-SUSP-COBR   OF GFCTB0E7.
           MOVE 'N'                    TO WRK-ACHOU-SUSPENSAO.
           MOVE GFCTVA-DOCOR-EVNTO     TO WRK-DATA-TABELA.

           EXEC SQL
             SELECT
                   T1.DINIC_SUSP_COBR,
                   T1.DFIM_SUSP_COBR
             INTO  :GFCTB0E7.DINIC-SUSP-COBR,
                   :GFCTB0E7.DFIM-SUSP-COBR
             FROM  DB2PRD.SUSP_TARIF_AGPTO T1,
                   DB2PRD.SUSP_GRP_CLI     T2
             WHERE T1.CSERVC_TARIF     = T2.CSERVC_TARIF           AND
                   T1.CAGPTO_CTA       = T2.CAGPTO_CTA             AND
                   T1.DINIC_SUSP_COBR  = T2.DINIC_SUSP_COBR        AND
                   T1.CSEQ_AGPTO_CTA   = T2.CSEQ_AGPTO_CTA         AND
                   T1.CAGPTO_CTA       = :GFCTB0E7.CAGPTO-CTA      AND
                   T1.CSERVC_TARIF     = :GFCTB0E7.CSERVC-TARIF    AND
                   T2.CCGC_CPF         = :GFCTB0E3.CCGC-CPF        AND
                   T2.CFLIAL_CGC       = :GFCTB0E3.CFLIAL-CGC      AND
                   T2.CCTRL_CPF_CGC    = :GFCTB0E3.CCTRL-CPF-CGC   AND
                   T1.DINIC_SUSP_COBR <= :WRK-DATA-TABELA           AND
                   T1.DFIM_SUSP_COBR  >= :WRK-DATA-TABELA
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS AND +100) OR
              (SQLWARN0                EQUAL 'W')
               MOVE 'DB2'              TO ERR-TIPO-ACESSO
               MOVE 'SUSP_GRP_CLI'     TO ERR-DBD-TAB
               MOVE 'SELECT  '         TO ERR-FUN-COMANDO
               MOVE  SQLCODE           TO ERR-SQL-CODE
               MOVE  '0010'            TO ERR-LOCAL
               MOVE  SPACES            TO ERR-SEGM
               PERFORM 9999-PROCESSAR-ROTINA-ERRO
           END-IF.

           IF (SQLCODE                 EQUAL ZEROS)
               MOVE 'S'                TO WRK-ACHOU-SUSPENSAO
               MOVE DINIC-SUSP-COBR    OF GFCTB0E7
                                       TO WRK-DINIC-AUX
               MOVE WRK-DIA-DINIC-AUX-R
                                       TO WRK-DIA-DINIC-INV-R
               MOVE WRK-MES-DINIC-AUX-R
                                       TO WRK-MES-DINIC-INV-R
               MOVE WRK-ANO-DINIC-AUX-R
                                       TO WRK-ANO-DINIC-INV-R
               MOVE DFIM-SUSP-COBR     OF GFCTB0E7
                                       TO WRK-DFIM-AUX
               MOVE WRK-DIA-DFIM-AUX-R TO WRK-DIA-DFIM-INV-R
               MOVE WRK-MES-DFIM-AUX-R TO WRK-MES-DFIM-INV-R
               MOVE WRK-ANO-DFIM-AUX-R TO WRK-ANO-DFIM-INV-R
           END-IF.

           MOVE WRK-CH-MOVEVNTO        TO WRK-CH-ANT.

      *----------------------------------------------------------------*
       3100-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      * GRAVACAO EVENTOS.                                              *
      ******************************************************************
      *----------------------------------------------------------------*
       3200-GRAVAR-EVENTOAT            SECTION.
      *----------------------------------------------------------------*

           WRITE FD-EVENTOAT           FROM GFCTVA-GFCTB092.

           MOVE WRK-GRAVACAO           TO WRK-OPERACAO.

           PERFORM 1120-TESTAR-FS-EVENTOAT.

           ADD 1                       TO ACU-GRAVADOS-EVENTOAT.

      *----------------------------------------------------------------*
       3200-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *    ROTINA PARA GRAVACAO DO ARQUIVO SUSPENSO                    *
      ******************************************************************
      *----------------------------------------------------------------*
       3300-GRAVAR-SUSPENSO            SECTION.
      *----------------------------------------------------------------*

           WRITE FD-SUSPENSO           FROM GFCTVA-GFCTB092.

           MOVE WRK-GRAVACAO           TO WRK-OPERACAO.

           PERFORM 1130-TESTAR-FS-SUSPENSO.

           ADD 1                       TO ACU-GRAVADOS-SUSPENSO.

      *----------------------------------------------------------------*
       3300-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      * FINALIZAR PROGRAMA.                                            *
      ******************************************************************
      *----------------------------------------------------------------*
       4000-FINALIZAR                  SECTION.
      *----------------------------------------------------------------*

           IF (ACU-GRAVADOS-EVENTOAT   NOT EQUAL ZEROS) OR
              (ACU-GRAVADOS-SUSPENSO   NOT EQUAL ZEROS)
               PERFORM 4100-EMITIR-DISPLAY-SUCESSO
               PERFORM 4200-EMITIR-DISPLAY-TOTAIS
           ELSE
               PERFORM 4300-EMITIR-DISPLAY-VAZIO
           END-IF.

           CLOSE EVENTO92
                 SUSPENSO
                 EVENTOAT.

           MOVE WRK-FECHAMENTO         TO WRK-OPERACAO.

           PERFORM 1100-TESTAR-FILE-STATUS

           STOP RUN.

      *----------------------------------------------------------------*
       4000-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      * EMITIR DISPLAY DE SUCESSO SE HOUVEREM GRAVACOES                *
      ******************************************************************
      *----------------------------------------------------------------*
       4100-EMITIR-DISPLAY-SUCESSO     SECTION.
      *----------------------------------------------------------------*

           DISPLAY '*------------------------------------------------*'.
           DISPLAY '*               PROGRAMA GFCT1894                *'.
           DISPLAY '*               -----------------                *'.
           DISPLAY '*                                                *'.
           DISPLAY '*       PROCESSAMENTO EFETUADO COM SUCESSO       *'.
           DISPLAY '*------------------------------------------------*'.

      *----------------------------------------------------------------*
       4100-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      * EMITIR DISPLAY DE TOTAIS                                       *
      ******************************************************************
      *----------------------------------------------------------------*
       4200-EMITIR-DISPLAY-TOTAIS      SECTION.
      *----------------------------------------------------------------*

           DISPLAY '****************************************************
      -            '***********'.

           MOVE ACU-LIDOS-EVENTO92     TO WRK-MASCARA.

           DISPLAY '*  TOTAL DE REGISTROS LIDOS     EM EVENTO92  = '
                     WRK-MASCARA          '  *'.

           MOVE ACU-GRAVADOS-EVENTOAT  TO WRK-MASCARA.

           DISPLAY '*  TOTAL DE REGISTROS GRAVADOS  EM EVENTOAT  = '
                     WRK-MASCARA          '  *'.

           MOVE ACU-GRAVADOS-SUSPENSO  TO WRK-MASCARA.

           DISPLAY '*  TOTAL DE REGISTROS GRAVADOS  EM SUSPENSO  = '
                     WRK-MASCARA          '  *'.

           DISPLAY '****************************************************
      -            '***********'.

      *----------------------------------------------------------------*
       4200-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      * EMITIR DISPLAY DE VAZIO                                        *
      ******************************************************************
      *----------------------------------------------------------------*
       4300-EMITIR-DISPLAY-VAZIO       SECTION.
      *----------------------------------------------------------------*

           DISPLAY '*------------------------------------------------*'.
           DISPLAY '*               PROGRAMA GFCT1894                *'.
           DISPLAY '*               -----------------                *'.
           DISPLAY '*                                                *'.
           DISPLAY '*          ARQUIVO DE  EVENTOS92 VAZIO           *'.
           DISPLAY '*------------------------------------------------*'.

      *----------------------------------------------------------------*
       4300-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      * PROCESSAR QUANDO HOUVER ERRO NO PROGRAMA                       *
      ******************************************************************
      *----------------------------------------------------------------*
       9999-PROCESSAR-ROTINA-ERRO      SECTION.
      *----------------------------------------------------------------*

           MOVE 'GFCT1894'             TO ERR-PGM.

           CALL 'POOL7100'             USING WRK-BATCH
                                             ERRO-AREA
                                             SQLCA.

           GOBACK.

      *----------------------------------------------------------------*
       9999-99-FIM.                    EXIT.
      *----------------------------------------------------------------*
