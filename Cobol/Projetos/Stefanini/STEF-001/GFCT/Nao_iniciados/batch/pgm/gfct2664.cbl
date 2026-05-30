      *================================================================*
       IDENTIFICATION                  DIVISION.
      *================================================================*
       PROGRAM-ID. GFCT2664.
      *================================================================*
      *                    S O N D A   P R O C W O R K                 *
      *----------------------------------------------------------------*
      *    PROGRAMA....:   GFCT2664                - BASE NO GFCT2112  *
      *    ANALISTA....:   PREVEDEL                - PROCWORK / GP.50  *
      *    DATA........:   15/12/2008                                  *
      *----------------------------------------------------------------*
      *    OBJETIVO....:   IDENTIFICA NO MESTRE DE CLIENTES QUEM EH    *
      *      AGRUP. BRADESCO EXPRESSO.                                 *
      *----------------------------------------------------------------*
      *    ARQUIVOS....:                                               *
      *                DDNAME                      INCLUDE/BOOK        *
      *                    EXMSTCLI                  GFCTWAET          *
      *                    EXSEMPAB                  GFCTWAET          *
      *                    EXCLIPAB                  GFCTWAET          *
      *----------------------------------------------------------------*
      *    BOOK'S......:                                               *
      *    POL7100C - AREA PARA TRATAMENTO DE ERROS PELA POOL7100.     *
      *    GFCTWAET - ARQUIVOS ENTRADA E SAIDA.                        *
      *    I#GFCTJM - AREA DE COMUNICACAO COM O MODULO GFCT5534.       *
      *    I#GFCT0M - AREA DE COMUNICACAO COM O MODULO GFCT5534        *
      *               PARA TRATAMENTO DE ERRO.                         *
      *----------------------------------------------------------------*
      *    MODULOS.....:                                               *
      *    GFCT5519 - VERIFICA BANCO POSTAL                            *
      *    POOL7100 - TRATAMENTO DE ERROS PADRAO BRADESCO.             *
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

           SELECT EXMSTCLI ASSIGN      TO UT-S-EXMSTCLI
           FILE STATUS                 IS WRK-FS-EXMSTCLI.

           SELECT EXSEMPAB  ASSIGN      TO UT-S-EXSEMPAB
           FILE STATUS                 IS WRK-FS-EXSEMPAB.

           SELECT EXCLIPAB  ASSIGN      TO UT-S-EXCLIPAB
           FILE STATUS                 IS WRK-FS-EXCLIPAB.

      *================================================================*
       DATA                            DIVISION.
      *================================================================*

      *----------------------------------------------------------------*
       FILE                            SECTION.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *    INPUT : ARQUIVO DE CLIENTES - ENTRADA                       *
      *            ORG. SEQUENCIAL     -   LRECL = 080                 *
      *----------------------------------------------------------------*

       FD  EXMSTCLI
           RECORDING MODE IS F
           LABEL RECORD IS STANDARD
           BLOCK CONTAINS 0 RECORDS.

       COPY GFCTWAET.

      *----------------------------------------------------------------*
      *    OUTPUT: ARQUIVO DE CLIENTES - SAIDA                         *
      *            ORG. SEQUENCIAL     -   LRECL = 080                 *
      *----------------------------------------------------------------*

       FD  EXSEMPAB
           RECORDING MODE IS F
           LABEL RECORD IS STANDARD
           BLOCK CONTAINS 0 RECORDS.

       01  FD-EXSEMPAB-REG                PIC  X(080).

      *----------------------------------------------------------------*
      *    OUTPUT: ARQUIVO DE CLIENTES - SAIDA                         *
      *            ORG. SEQUENCIAL     -   LRECL = 080                 *
      *----------------------------------------------------------------*

       FD  EXCLIPAB
           RECORDING MODE IS F
           LABEL RECORD IS STANDARD
           BLOCK CONTAINS 0 RECORDS.

       01  FD-EXCLIPAB-REG                PIC  X(080).

      *----------------------------------------------------------------*
       WORKING-STORAGE                 SECTION.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       77  FILLER                      PIC  X(050)         VALUE
           '* INICIO DA WORKING STORAGE SECTION *'.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       77  FILLER                      PIC  X(050)         VALUE
           '* AREA DE ACUMULADORES *'.
      *----------------------------------------------------------------*

       77  ACU-LIDOS-EXMSTCLI          PIC  9(009) COMP-3  VALUE ZEROS.
       77  ACU-BPST-PROCESSADO         PIC  9(009) COMP-3  VALUE ZEROS.
       77  ACU-GRAVADOS-EXSEMPAB       PIC  9(009) COMP-3  VALUE ZEROS.
       77  ACU-GRAVADOS-EXCLIPAB       PIC  9(009) COMP-3  VALUE ZEROS.
       77  ACU-BANCOPOSTAL             PIC  9(009) COMP-3  VALUE ZEROS.
       77  ACU-BRADESCOEXPRESSO        PIC  9(009) COMP-3  VALUE ZEROS.
       77  ACU-PABSIMPLES              PIC  9(009) COMP-3  VALUE ZEROS.
       77  ACU-POSTO-NAO-CADASTRADO    PIC  9(009) COMP-3  VALUE ZEROS.
       77  ACU-VERIFICADOS             PIC  9(009) COMP-3  VALUE ZEROS.

      *----------------------------------------------------------------*
       77  FILLER                      PIC  X(050)         VALUE
           '* AREA DE TESTES DE FILE STATUS *'.
      *----------------------------------------------------------------*

       77  WRK-FS-EXMSTCLI             PIC  X(002)         VALUE SPACES.
       77  WRK-FS-EXSEMPAB             PIC  X(002)         VALUE SPACES.
       77  WRK-FS-EXCLIPAB             PIC  X(002)         VALUE SPACES.

       77  WRK-ABERTURA                PIC  X(013)         VALUE
           ' NA ABERTURA '.
       77  WRK-LEITURA                 PIC  X(013)         VALUE
           ' NA  LEITURA '.
       77  WRK-GRAVACAO                PIC  X(013)         VALUE
           ' NA GRAVACAO '.
       77  WRK-FECHAMENTO              PIC  X(013)         VALUE
           'NO FECHAMENTO'.

      *----------------------------------------------------------------*
       77  FILLER                      PIC  X(050)         VALUE
           '* AREA PARA VARIAVEIS AUXILIARES *'.
      *----------------------------------------------------------------*

       77  WRK-MODULO                  PIC  X(008)         VALUE SPACES.
       77  WRK-BATCH                   PIC  X(008)         VALUE
           'BATCH'.
       77  WRK-MASCARA                 PIC  ZZZ.ZZZ.ZZ9    VALUE ZEROS.

       77  WRK-BANCO-POSTAL            PIC  X(001)         VALUE 'N'.
       77  WRK-BRADESCO-EXPRESSO       PIC  X(001)         VALUE 'N'.
       77  WRK-CONTADOR                PIC  9(002) COMP-3  VALUE ZEROS.

       01  WRK-BPOSTAL-ATU.
           05  WRK-AGENCIA-ATU         PIC S9(005)         VALUE ZEROS.
           05  WRK-PAB-ATU             PIC S9(005)         VALUE ZEROS.

       01  WRK-BPOSTAL-ANT.
           05  WRK-AGENCIA-ANT         PIC S9(005)         VALUE ZEROS.
           05  WRK-PAB-ANT             PIC S9(005)         VALUE ZEROS.

       01  WRK-AGENCIA-S               PIC S9(005)         VALUE ZEROS.
       01  FILLER                      REDEFINES WRK-AGENCIA-S.
           05 WRK-AGENCIA              PIC  9(005).

       01  WRK-PAB-S                   PIC S9(005)         VALUE ZEROS.
       01  FILLER                      REDEFINES WRK-PAB-S.
           05 FILLER                   PIC  9(002).
           05 WRK-PAB                  PIC  9(003).

      *----------------------------------------------------------------*
       77  FILLER                      PIC  X(050)         VALUE
           '* AREA PARA MENSAGENS *'.
      *----------------------------------------------------------------*

       77  WRK-MSG001                  PIC  X(058)         VALUE
           '** ARQUIVO EXMSTCLI ESTA VAZIO, PROCESSAMENTO CANCELADO **'.

      *----------------------------------------------------------------*
       77  FILLER                      PIC  X(050)         VALUE
           '* MENSAGEM DE ERRO DE FILE-STATUS *'.
      *----------------------------------------------------------------*

       01  WRK-ERRO-TEXTO.
           05 FILLER                   PIC  X(008)         VALUE
              '** ERRO '.
           05 WRK-OPERACAO             PIC  X(013)         VALUE SPACES.
           05 FILLER                   PIC  X(012)         VALUE
              ' DO ARQUIVO '.
           05 WRK-NOME-ARQ             PIC  X(008)         VALUE SPACES.
           05 FILLER                   PIC  X(017)         VALUE
              ' - FILE STATUS = '.
           05 WRK-FILE-STATUS          PIC  X(002)         VALUE SPACES.
           05 FILLER                   PIC  X(003)         VALUE ' **'.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           '* AREA PARA TRATAMENTO DE ERRO *'.
      *----------------------------------------------------------------*

       COPY POL7100C.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           '* AREA DE COMUNICACAO COM O MODULO GFCT5519 *'.
      *----------------------------------------------------------------*

       COPY GFCTWABE.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           '* FIM DA WORKING STORAGE SECTION *'.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       LINKAGE                         SECTION.
      *----------------------------------------------------------------*

       01  LNK-PARM.
           05  LNK-TAM                 PIC S9(004)         COMP.
           05  LNK-PAR                 PIC  X(001).

      *================================================================*
       PROCEDURE                       DIVISION   USING   LNK-PARM.
      *================================================================*

      ******************************************************************
      *    CHAMA AS ROTINAS A SEREM PROCESSADAS                        *
      ******************************************************************
      *----------------------------------------------------------------*
       0000-INICIAR                    SECTION.
      *----------------------------------------------------------------*

           PERFORM 1000-INICIALIZAR.

           PERFORM 2000-VERIFICAR-VAZIO.

           PERFORM 3000-PROCESSAR      UNTIL
                   WRK-FS-EXMSTCLI     EQUAL '10'.
           PERFORM 4000-FINALIZAR.

      *----------------------------------------------------------------*
       0000-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *    ABERTURA DE ARQUIVOS                                        *
      ******************************************************************
      *----------------------------------------------------------------*
       1000-INICIALIZAR                SECTION.
      *----------------------------------------------------------------*

           IF  LNK-PAR NOT EQUAL '0' AND '1'
               DISPLAY '***** GFCT2664 *****'
               DISPLAY '*                  *'
               DISPLAY '*  PARM  INVALIDO  *'
               DISPLAY '*  PGM. CANCELADO  *'
               DISPLAY '*                  *'
               DISPLAY '***** GFCT2664 *****'
               MOVE 'APL'              TO ERR-TIPO-ACESSO
               MOVE 'PARM INVALIDO'    TO ERR-TEXTO
               PERFORM 9999-PROCESSAR-ROTINA-ERRO
           END-IF.

           OPEN INPUT  EXMSTCLI
                OUTPUT EXSEMPAB
                       EXCLIPAB.

           MOVE  WRK-ABERTURA          TO WRK-OPERACAO.

           PERFORM 1100-TESTAR-FILE-STATUS.

      *----------------------------------------------------------------*
       1000-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *    TESTE DE FILE STATUS DOS ARQUIVOS                           *
      ******************************************************************
      *----------------------------------------------------------------*
       1100-TESTAR-FILE-STATUS         SECTION.
      *----------------------------------------------------------------*

           PERFORM 1110-TESTAR-FS-EXMSTCLI.

           PERFORM 1120-TESTAR-FS-EXSEMPAB.

           PERFORM 1130-TESTAR-FS-EXCLIPAB.

      *----------------------------------------------------------------*
       1100-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *    TESTE DE FILE STATUS DO ARQUIVO EXMSTCLI                    *
      ******************************************************************
      *----------------------------------------------------------------*
       1110-TESTAR-FS-EXMSTCLI         SECTION.
      *----------------------------------------------------------------*

           IF  WRK-FS-EXMSTCLI         NOT EQUAL '00'
               MOVE 'APL'              TO ERR-TIPO-ACESSO
               MOVE WRK-FS-EXMSTCLI    TO WRK-FILE-STATUS
               MOVE 'EXMSTCLI'         TO WRK-NOME-ARQ
               MOVE WRK-ERRO-TEXTO     TO ERR-TEXTO
               PERFORM 9999-PROCESSAR-ROTINA-ERRO
           END-IF.

      *----------------------------------------------------------------*
       1110-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *    TESTE DE FILE STATUS DO ARQUIVO EXSEMPAB                    *
      ******************************************************************
      *----------------------------------------------------------------*
       1120-TESTAR-FS-EXSEMPAB          SECTION.
      *----------------------------------------------------------------*

           IF  WRK-FS-EXSEMPAB         NOT EQUAL '00'
               MOVE 'APL'              TO ERR-TIPO-ACESSO
               MOVE WRK-FS-EXSEMPAB    TO WRK-FILE-STATUS
               MOVE 'EXSEMPAB'         TO WRK-NOME-ARQ
               MOVE WRK-ERRO-TEXTO     TO ERR-TEXTO
               PERFORM 9999-PROCESSAR-ROTINA-ERRO
           END-IF.

      *----------------------------------------------------------------*
       1120-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *    TESTE DE FILE STATUS DO ARQUIVO EXCLIPAB                    *
      ******************************************************************
      *----------------------------------------------------------------*
       1130-TESTAR-FS-EXCLIPAB          SECTION.
      *----------------------------------------------------------------*

           IF  WRK-FS-EXCLIPAB         NOT EQUAL '00'
               MOVE 'APL'              TO ERR-TIPO-ACESSO
               MOVE WRK-FS-EXCLIPAB    TO WRK-FILE-STATUS
               MOVE 'EXCLIPAB'         TO WRK-NOME-ARQ
               MOVE WRK-ERRO-TEXTO     TO ERR-TEXTO
               PERFORM 9999-PROCESSAR-ROTINA-ERRO
           END-IF.

      *----------------------------------------------------------------*
       1130-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *    VERIFICA SE O ARQUIVO DE ENTRADA ESTA VAZIO                 *
      ******************************************************************
      *----------------------------------------------------------------*
       2000-VERIFICAR-VAZIO            SECTION.
      *----------------------------------------------------------------*

           PERFORM 2100-LER-EXMSTCLI.

           IF  WRK-FS-EXMSTCLI         EQUAL '10'
               DISPLAY '*************** GFCT2664 ***************'
               DISPLAY '*                                      *'
               DISPLAY '*      ARQUIVO EXMSTCLI ESTA VAZIO     *'
               DISPLAY '*                                      *'
               DISPLAY '*        PROCESSAMENTO CANCELADO       *'
               DISPLAY '*                                      *'
               DISPLAY '*************** GFCT2664 ***************'
           END-IF.

      *----------------------------------------------------------------*
       2000-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *    LEITURA DO ARQUIVO DE PARAMETROS EXMSTCLI                   *
      ******************************************************************
      *----------------------------------------------------------------*
       2100-LER-EXMSTCLI               SECTION.
      *----------------------------------------------------------------*

           READ EXMSTCLI.

           IF  WRK-FS-EXMSTCLI         EQUAL '10'
               GO TO 2100-99-FIM
           END-IF.

           MOVE WRK-LEITURA            TO WRK-OPERACAO.

           PERFORM 1110-TESTAR-FS-EXMSTCLI.

           MOVE  WAET-AGENCIA-PREV        TO WRK-AGENCIA-ATU.
           MOVE  WAET-PAB                 TO WRK-PAB-ATU.

           ADD 1                       TO ACU-LIDOS-EXMSTCLI.

      *----------------------------------------------------------------*
       2100-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *    PROCESSAMENTO PRINCIPAL PARA CADA REGISTRO DO EXMCSEGT
      ******************************************************************
      *----------------------------------------------------------------*
       3000-PROCESSAR                  SECTION.
      *----------------------------------------------------------------*

           IF WRK-BPOSTAL-ATU NOT EQUAL WRK-BPOSTAL-ANT
               PERFORM 3100-VERIFICAR-BANCO-POSTAL
               MOVE WRK-BPOSTAL-ATU TO WRK-BPOSTAL-ANT
               ADD 1                TO ACU-VERIFICADOS
           END-IF.

           ADD  1  TO  ACU-BPST-PROCESSADO.

           IF (WRK-BANCO-POSTAL      EQUAL 'S' AND
               LNK-PAR               EQUAL '0') OR
              (WRK-BRADESCO-EXPRESSO EQUAL 'S' AND
               LNK-PAR               EQUAL '1')
              PERFORM 3300-GRAVAR-EXCLIPAB
           ELSE
              PERFORM 3200-GRAVAR-EXSEMPAB
           END-IF.


           PERFORM 2100-LER-EXMSTCLI.

      *----------------------------------------------------------------*
       3000-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *    VERIFICAR BANCO POSTAL                                      *
      ******************************************************************
      *----------------------------------------------------------------*
       3100-VERIFICAR-BANCO-POSTAL            SECTION.
      *----------------------------------------------------------------*

           INITIALIZE WABE-AREA-GFCT5519.

           MOVE  WAET-AGENCIA-PREV        TO WRK-AGENCIA-S.
           MOVE  WRK-AGENCIA              TO WABE-CJUNC-DEPDC.
           MOVE  WAET-PAB                 TO WRK-PAB-S
           MOVE  WRK-PAB                  TO WABE-CPOSTO-SERVC.
           MOVE  'B'                      TO WABE-AMBIENTE.
           MOVE 'GFCT5519'                TO WRK-MODULO.
           CALL WRK-MODULO             USING WABE-AREA-GFCT5519.

           IF  RETURN-CODE             NOT EQUAL ZEROS
               DISPLAY '*********** GFCT2664 ***********'
               DISPLAY '*                              *'
               DISPLAY '*   ERRO NO ACESSO AO MODULO   *'
               DISPLAY '*           GFCT5519           *'
               DISPLAY '*                              *'
               DISPLAY '*********** GFCT2664 ***********'
               PERFORM 9999-PROCESSAR-ROTINA-ERRO
           END-IF.

           IF WABE-COD-RETORNO EQUAL 00 AND
              WABE-TIPO-POSTO  EQUAL 2
               MOVE 'S' TO WRK-BANCO-POSTAL
               MOVE 'N' TO WRK-BRADESCO-EXPRESSO
               ADD  1   TO ACU-BANCOPOSTAL
           ELSE
               IF WABE-COD-RETORNO EQUAL 00 AND
                  WABE-TIPO-POSTO  EQUAL 3
                   MOVE 'S' TO WRK-BRADESCO-EXPRESSO
                   MOVE 'N' TO WRK-BANCO-POSTAL
                   ADD  1   TO ACU-BRADESCOEXPRESSO
               ELSE
                   IF WABE-COD-RETORNO EQUAL 00 AND
                      WABE-TIPO-POSTO  EQUAL 1
                       MOVE 'N' TO WRK-BANCO-POSTAL
                       MOVE 'N' TO WRK-BRADESCO-EXPRESSO
                       ADD   1  TO ACU-PABSIMPLES
                   END-IF
               END-IF
           END-IF.

           IF WABE-COD-RETORNO EQUAL 05 OR 04
               MOVE 'N' TO WRK-BANCO-POSTAL
               MOVE 'N' TO WRK-BRADESCO-EXPRESSO
               ADD   1  TO ACU-POSTO-NAO-CADASTRADO
               IF WRK-CONTADOR NOT GREATER 9
                   DISPLAY
                   'AGENCIA NAO ENCONTRADA NO MESU/YMKC - '
                   'AGENCIA = ' WAET-AGENCIA-PREV ' / '
                   'POSTO = ' WAET-PAB
                   ADD 1 TO WRK-CONTADOR
               END-IF
           END-IF

           IF WABE-COD-RETORNO NOT EQUAL 00 AND 05 AND 04
               DISPLAY '*********** GFCT2664 ***********'
               DISPLAY '*                              *'
               DISPLAY '*   ERRO NO MODULO GFCT5519    *'
               DISPLAY '*   CODIGO ERRO: ' WABE-COD-RETORNO
                                          '            *'
               DISPLAY '*                              *'
               DISPLAY '*********** GFCT2664 ***********'
               MOVE WABE-DESC-MSG    TO ERR-TEXTO
               PERFORM 9999-PROCESSAR-ROTINA-ERRO
           END-IF.

      *----------------------------------------------------------------*
       3100-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *    GRAVACAO DO ARQUIVO EXSEMPAB
      ******************************************************************
      *----------------------------------------------------------------*
       3200-GRAVAR-EXSEMPAB             SECTION.
      *----------------------------------------------------------------*

           WRITE FD-EXSEMPAB-REG        FROM GFCTWAET-REG.

           MOVE WRK-GRAVACAO           TO WRK-OPERACAO.

           PERFORM 1120-TESTAR-FS-EXSEMPAB.

           ADD 1                       TO ACU-GRAVADOS-EXSEMPAB.

      *----------------------------------------------------------------*
       3200-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *    GRAVACAO DO ARQUIVO EXCLIPAB
      ******************************************************************
      *----------------------------------------------------------------*
       3300-GRAVAR-EXCLIPAB             SECTION.
      *----------------------------------------------------------------*

           WRITE FD-EXCLIPAB-REG        FROM GFCTWAET-REG.

           MOVE WRK-GRAVACAO           TO WRK-OPERACAO.

           PERFORM 1130-TESTAR-FS-EXCLIPAB.

           ADD 1                       TO ACU-GRAVADOS-EXCLIPAB.

      *----------------------------------------------------------------*
       3300-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *    PROCEDIMENTOS REALIZADOS NO FINAL DO PROCESSAMENTO          *
      ******************************************************************
      *----------------------------------------------------------------*
       4000-FINALIZAR                  SECTION.
      *----------------------------------------------------------------*

           PERFORM 4100-EMITIR-DISP-FINAL.

           CLOSE EXMSTCLI
                 EXSEMPAB
                 EXCLIPAB.

           MOVE WRK-FECHAMENTO         TO WRK-OPERACAO.

           PERFORM 1100-TESTAR-FILE-STATUS.

           STOP RUN.

      *----------------------------------------------------------------*
       4000-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *    EMITE TOTALIZADOR FINAL                                     *
      ******************************************************************
      *----------------------------------------------------------------*
       4100-EMITIR-DISP-FINAL          SECTION.
      *----------------------------------------------------------------*

           DISPLAY '******************* GFCT2664 *******************'.
           DISPLAY '*                                              *'.
           MOVE ACU-LIDOS-EXMSTCLI     TO WRK-MASCARA.
           DISPLAY '* TOTAIS DE REG. LIDOS EXMSTCLI..: '
                   WRK-MASCARA                                   ' *'.
           DISPLAY '*                                              *'.
           MOVE ACU-BPST-PROCESSADO    TO WRK-MASCARA.
           DISPLAY '* TOTAIS DE REG. PROCESSADOS.....: '
                   WRK-MASCARA                                   ' *'.
           DISPLAY '*                                              *'.
           MOVE ACU-VERIFICADOS TO WRK-MASCARA.
           DISPLAY '* TOTAIS DE POSTOS PESQUISADOS     '
                   WRK-MASCARA                                   ' *'.
           DISPLAY '*                                              *'.
           MOVE ACU-BANCOPOSTAL TO WRK-MASCARA.
           DISPLAY '* TOTAIS DE BANCO POSTAL - GFCT    '
                   WRK-MASCARA                                   ' *'.
           DISPLAY '*                                              *'.
           MOVE ACU-BRADESCOEXPRESSO TO WRK-MASCARA.
           DISPLAY '* TOTAIS DE BRAD. EXPRESSO - GFCT  '
                   WRK-MASCARA                                   ' *'.
           DISPLAY '*                                              *'.
           MOVE ACU-PABSIMPLES TO WRK-MASCARA.
           DISPLAY '* TOTAIS DE PAB SIMPLES - GFCT     '
                   WRK-MASCARA                                   ' *'.
           DISPLAY '*                                              *'.
           MOVE ACU-POSTO-NAO-CADASTRADO TO WRK-MASCARA.
           DISPLAY '* TOTAIS DE PAB NAO CADAST. - GFCT '
                   WRK-MASCARA                                   ' *'.
           DISPLAY '*                                              *'.
           MOVE ACU-GRAVADOS-EXSEMPAB   TO WRK-MASCARA.
           DISPLAY '* TOTAIS DE REG.GRAVADOS EXSEMPAB: '
                   WRK-MASCARA                                   ' *'.
           DISPLAY '*                                              *'.
           MOVE ACU-GRAVADOS-EXCLIPAB   TO WRK-MASCARA.
           DISPLAY '* TOTAIS DE REG.GRAVADOS EXCLIPAB: '
                   WRK-MASCARA                                   ' *'.
           DISPLAY '*                                              *'.
           DISPLAY '******************* GFCT2664 *******************'.

      *----------------------------------------------------------------*
       4100-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *    EXECUTA PROCEDIMENTO DE CANCELAMENTO DO PGM EM CASO DE ERRO *
      ******************************************************************
      *----------------------------------------------------------------*
       9999-PROCESSAR-ROTINA-ERRO      SECTION.
      *----------------------------------------------------------------*

           MOVE 'GFCT2664'             TO ERR-PGM.
           MOVE 04                     TO RETURN-CODE.

           CALL 'POOL7100'             USING WRK-BATCH
                                             ERRO-AREA.

           GOBACK.

      *----------------------------------------------------------------*
       9999-99-FIM.                    EXIT.
      *----------------------------------------------------------------*
