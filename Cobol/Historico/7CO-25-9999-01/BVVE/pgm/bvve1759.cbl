      *================================================================*
       IDENTIFICATION                  DIVISION.
      *================================================================*

       PROGRAM-ID. BVVE1759.
       AUTHOR.     MARCOS.

      *================================================================*
      *                       F  O  U  R  S  Y  S                      *
      *================================================================*
      *    PROGRAMA......: BVVE1759                                    *
      *    PROGRAMADOR...: MARCOS TADEU           - FOURSYS            *
      *    DATA..........: 29/06/2018                                  *
      *----------------------------------------------------------------*
      *    PROJETO.......: PROPOSTA ELETRONICA FASE II PAGAMENTO PRAZO *
      *----------------------------------------------------------------*
      *    OBJETIVO......: MODULO DE MANUTENCAO DE PARAMETRIZACAO      *
      *                    DE CNPJS RESTRICAO                          *
      *----------------------------------------------------------------*
      *    ARQUIVOS......: DDNAME        LRECL   INCLUDE/BOOK          *
      *                   VVCNPJRE        150      I#BVVECN            *
      *                   SPCNPJRE        150      I#BVVESC            *
      *----------------------------------------------------------------*
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
       01  FILLER                      PIC  X(050)         VALUE
           '*** INICIO DA WORKING BVVE1758 ***'.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           '*** AREA DE MENSAGENS ***'.
      *----------------------------------------------------------------*

       01  WRK-MENSAGENS.
           03  WRK-MENSAGEM            PIC  X(080)         VALUE SPACES.

      *    MENSAGENS DE CONSISTENCIAS
           03  WRK-MSG01               PIC  X(080)         VALUE
               '01 - TIPO MANUTENCAO INVALIDO'.
           03  WRK-MSG02               PIC  X(080)         VALUE
               '02 - EXCLUSAO SOLICITACAO INVALIDO'.
           03  WRK-MSG03               PIC  X(080)         VALUE
               '03 - CODIGO DO CNPJ INVALIDO'.
           03  WRK-MSG04               PIC  X(080)         VALUE
               '05 - DT.INCLUSAO INVALIDA'.
           03  WRK-MSG05               PIC  X(080)         VALUE
               '06 - USUARIO INVALIDO'.

      *    MENSAGENS DE ACESSOS VSAM
           03  WRK-MSG-VSAM-01         PIC  X(080)         VALUE
               '01 - CONSULTA - REGISTRO NAO ENCONTRADO'.
           03  WRK-MSG-VSAM-02         PIC  X(080)         VALUE
               '02 - INCLUSAO -REGISTRO DUPLICADO'.
           03  WRK-MSG-VSAM-03         PIC  X(080)         VALUE
               '03 - ATUALIZACAO - REGISTRO NAO ENCONTRADO'.
           03  WRK-MSG-VSAM-04         PIC  X(080)         VALUE
               '04 - EXCLUSAO - REGISTRO NAO ENCONTRADO'.

      *    MENSAGENS DE TRATAMENTO DE ERRO
           03  WRK-MSG99.
               05  FILLER              PIC  X(035)         VALUE
                   'ERRO NAO IDENTIF PELO PGM. EIBRESP='.
               05  WRK-EIBRESP         PIC  9(004)         VALUE ZEROS.
               05  FILLER              PIC  X(009)         VALUE
                   ' E LOCAL='.
               05  WRK-LOCAL-EIBRESP   PIC  9(002)         VALUE ZEROS.
               05  FILLER              PIC  X(020)         VALUE
                   '. CONTACTAR ANALISTA'.

           03  WRK-MSG00.
               05  FILLER              PIC  X(019)         VALUE
                   'ERRO CICS - ABCODE='.
               05  WRK-ABCODE-MSG      PIC  X(008)         VALUE SPACES.
               05  FILLER              PIC  X(009)         VALUE
                   ' E LOCAL='.
               05  WRK-LOCAL-ABCODE    PIC  9(002)         VALUE ZEROS.
               05  FILLER              PIC  X(030)         VALUE
                   '. CONTACTAR ANALISTA'.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           '*** COMMAREA AUXILIAR ***'.
      *----------------------------------------------------------------*

       01  WRK-COMMAREA.
           03  WRK-COMM-ENTRADA.
               05  WRK-COMM-TP-MANUT   PIC  X(001)         VALUE SPACES.
               05  WRK-COMM-EXC-SOLCT  PIC  X(001)         VALUE SPACES.
               05  WRK-COMM-AREA-VSAM  PIC  X(100)         VALUE SPACES.
           03  WRK-COMM-SAIDA.
               05  WRK-COMM-RC         PIC  9(002)         VALUE ZEROS.
               05  WRK-COMM-MENSAGEM   PIC  X(080)         VALUE SPACES.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(32)        VALUE
          '*   AREA DO ARQUIVO VVCNPJRE  *'.
      *----------------------------------------------------------------*

           COPY 'I#BVVECN'.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(32)        VALUE
           '*   AREA DO ARQUIVO SPCNPJRE  *'.
      *----------------------------------------------------------------*

           COPY 'I#BVVESC'.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           '***  FIM DA WORKING BVVE1758 ***'.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       LINKAGE                         SECTION.
      *----------------------------------------------------------------*

       01  DFHCOMMAREA.
           03  FILLER                  PIC  X(184).

      *================================================================*
       PROCEDURE                       DIVISION USING DFHCOMMAREA.
      *================================================================*

      ******************************************************************
      *    ROTINA PRINCIPAL                                            *
      ******************************************************************
      *----------------------------------------------------------------*
       0000-PRINCIPAL                    SECTION.
      *----------------------------------------------------------------*

           PERFORM 1000-INICIAR.
           PERFORM 2000-PROCESSAR.
           PERFORM 3000-FINALIZAR.

      *----------------------------------------------------------------*
       0000-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *    TRATAMENTOS INICIAIS DO PROGRAMA                            *
      ******************************************************************
      *----------------------------------------------------------------*
       1000-INICIAR                    SECTION.
      *----------------------------------------------------------------*

      *    TRATAMENTO PADRAO EM CASO DE ERRO
           MOVE 01                     TO WRK-LOCAL-EIBRESP
                                          WRK-LOCAL-ABCODE.

           EXEC CICS
                HANDLE ABEND
                       LABEL(9998-TRATAR-ERRO)
           END-EXEC.

           IF EIBRESP                  NOT EQUAL DFHRESP(NORMAL)
              PERFORM 9998-TRATAR-ERRO
           END-IF.

           MOVE DFHCOMMAREA(1:EIBCALEN) TO WRK-COMMAREA.
           MOVE WRK-COMM-AREA-VSAM      TO PARM-CNPJ-RESTRICAO.
           MOVE SPACES                  TO PARM-CNPJ-FILLER
           MOVE ZEROS                   TO WRK-COMM-RC.

           PERFORM 1100-CONSISTIR-ENTRADA.

      *----------------------------------------------------------------*
       1000-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *    CONSISTENCIA DO DADOS RECEBIDOS                             *
      ******************************************************************
      *----------------------------------------------------------------*
       1100-CONSISTIR-ENTRADA          SECTION.
      *----------------------------------------------------------------*

           IF WRK-COMM-TP-MANUT        NOT EQUAL 'C'  AND 'I'
                                             AND 'A'  AND 'E'
              MOVE 98                  TO WRK-COMM-RC
              MOVE WRK-MSG01           TO WRK-MENSAGEM
              PERFORM 3000-FINALIZAR
           END-IF.

           IF WRK-COMM-EXC-SOLCT       NOT EQUAL 'S' AND 'N'
              MOVE 98                  TO WRK-COMM-RC
              MOVE WRK-MSG02           TO WRK-MENSAGEM
              PERFORM 3000-FINALIZAR
           END-IF.

      *    VALIDACAO DA CHAVE DO VSAM
7C2511*    IF PARM-CNPJ-RESTRICAO-CH   NOT NUMERIC OR
7C2511*       PARM-CNPJ-RESTRICAO-CH   EQUAL ZEROS
7C2511     IF PARM-CNPJ-RESTRICAO-CH   EQUAL SPACES OR
7C2511        PARM-CNPJ-RESTRICAO-CH   EQUAL LOW-VALUES
              MOVE 98                  TO WRK-COMM-RC
              MOVE WRK-MSG03           TO WRK-MENSAGEM
              PERFORM 3000-FINALIZAR
           END-IF.

      *    VALIDACAO DOS DEMAIS CAMPOS DO VSAM
           IF WRK-COMM-TP-MANUT        EQUAL 'I' OR 'A'
              IF PARM-CNPJ-REST-DT-INCLUSAO
                                       NOT NUMERIC OR
                 PARM-CNPJ-REST-DT-INCLUSAO
                                       EQUAL ZEROS
                 MOVE 98               TO WRK-COMM-RC
                 MOVE WRK-MSG04        TO WRK-MENSAGEM
                 PERFORM 3000-FINALIZAR
              END-IF

              IF PARM-CNPJ-REST-USUARIO
                                       EQUAL SPACES
                 MOVE 98               TO WRK-COMM-RC
                 MOVE WRK-MSG05        TO WRK-MENSAGEM
                 PERFORM 3000-FINALIZAR
              END-IF
           END-IF.

      *----------------------------------------------------------------*
       1100-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *    PROCESSAMENTO PRINCIPAL                                     *
      ******************************************************************
      *----------------------------------------------------------------*
       2000-PROCESSAR                  SECTION.
      *----------------------------------------------------------------*

           EVALUATE WRK-COMM-TP-MANUT
               WHEN 'C'  PERFORM 2100-CONSULTAR-PARAMETRIZACAO
               WHEN 'I'  PERFORM 2200-INCLUIR-PARAMETRIZACAO
               WHEN 'A'  PERFORM 2300-ALTERAR-PARAMETRIZACAO
               WHEN 'E'  PERFORM 2400-EXCLUIR-PARAMETRIZACAO
           END-EVALUATE.

           IF WRK-COMM-EXC-SOLCT       EQUAL 'S'
              PERFORM 2500-EXCLUIR-SOLICITACAO
           END-IF.

      *----------------------------------------------------------------*
       2000-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *    REALIZA CONSULTA DA PARAMETRIZACAO                          *
      ******************************************************************
      *----------------------------------------------------------------*
       2100-CONSULTAR-PARAMETRIZACAO   SECTION.
      *----------------------------------------------------------------*

           MOVE 02                     TO WRK-LOCAL-EIBRESP
                                          WRK-LOCAL-ABCODE.

           EXEC CICS
                READ FILE ('VVCNPJRE')
                     INTO (PARM-CNPJ-RESTRICAO)
                   RIDFLD (PARM-CNPJ-REST-CH)
                   NOHANDLE
           END-EXEC.

           IF EIBRESP                  NOT EQUAL DFHRESP(NORMAL) AND
                                                 DFHRESP(NOTFND)
              PERFORM 9998-TRATAR-ERRO
           END-IF.

           IF EIBRESP                  EQUAL DFHRESP(NOTFND)
              MOVE 01                  TO WRK-COMM-RC
              MOVE WRK-MSG-VSAM-01     TO WRK-MENSAGEM
           END-IF.

      *----------------------------------------------------------------*
       2100-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *    REALIZA INCLUSAO DA PARAMETRIZACAO                          *
      ******************************************************************
      *----------------------------------------------------------------*
       2200-INCLUIR-PARAMETRIZACAO     SECTION.
      *----------------------------------------------------------------*

           MOVE 03                     TO WRK-LOCAL-EIBRESP
                                          WRK-LOCAL-ABCODE.

           EXEC CICS
                WRITE DATASET ('VVCNPJRE')
                         FROM (PARM-CNPJ-RESTRICAO)
                       RIDFLD (PARM-CNPJ-REST-CH)
                       NOHANDLE
           END-EXEC.

           IF EIBRESP                  NOT EQUAL DFHRESP(NORMAL) AND
                                                 DFHRESP(DUPREC)
              PERFORM 9998-TRATAR-ERRO
           END-IF.

           IF EIBRESP                  EQUAL DFHRESP(DUPREC)
              MOVE 99                  TO WRK-COMM-RC
              MOVE WRK-MSG-VSAM-02     TO WRK-MENSAGEM
              PERFORM 3000-FINALIZAR
           END-IF.

      *----------------------------------------------------------------*
       2200-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *    REALIZA ALTERACAO DA PARAMETRIZACAO                         *
      ******************************************************************
      *----------------------------------------------------------------*
       2300-ALTERAR-PARAMETRIZACAO     SECTION.
      *----------------------------------------------------------------*

      *    LEITURA PARA ATUALIZACAO
           MOVE 04                     TO WRK-LOCAL-EIBRESP
                                          WRK-LOCAL-ABCODE.

           EXEC CICS
                READ FILE   ('VVCNPJRE')
                     INTO   (SPARM-CNPJ-RESTRICAO)
                     RIDFLD (PARM-CNPJ-REST-CH)
                     UPDATE
                     NOHANDLE
           END-EXEC.

           IF EIBRESP                  NOT EQUAL DFHRESP(NORMAL) AND
                                                 DFHRESP(NOTFND)
              PERFORM 9998-TRATAR-ERRO
           END-IF.

           IF EIBRESP                  EQUAL DFHRESP(NOTFND)
              MOVE 99                  TO WRK-COMM-RC
              MOVE WRK-MSG-VSAM-03     TO WRK-MENSAGEM
              PERFORM 3000-FINALIZAR
           END-IF.

      *    ATUALIZACAO
           MOVE 05                     TO WRK-LOCAL-EIBRESP
                                          WRK-LOCAL-ABCODE.

           EXEC CICS
                REWRITE DATASET ('VVCNPJRE')
                           FROM (PARM-CNPJ-RESTRICAO)
                           NOHANDLE

           END-EXEC.

           IF EIBRESP                  NOT EQUAL DFHRESP(NORMAL)
               PERFORM 9998-TRATAR-ERRO
           END-IF.

      *----------------------------------------------------------------*
       2300-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *    REALIZA EXCLUSAO DA SOLICITACAO                             *
      ******************************************************************
      *----------------------------------------------------------------*
       2400-EXCLUIR-PARAMETRIZACAO     SECTION.
      *----------------------------------------------------------------*

           MOVE 06                     TO WRK-LOCAL-EIBRESP
                                          WRK-LOCAL-ABCODE.

           EXEC CICS
                DELETE DATASET ('VVCNPJRE')
                       RIDFLD  (PARM-CNPJ-REST-CH)
                       NOHANDLE
           END-EXEC.

           IF EIBRESP                  NOT EQUAL DFHRESP(NORMAL) AND
                                                 DFHRESP(NOTFND)
              PERFORM 9998-TRATAR-ERRO
           END-IF.

           IF EIBRESP                  EQUAL DFHRESP(NOTFND)
              MOVE 99                  TO WRK-COMM-RC
              MOVE WRK-MSG-VSAM-04     TO WRK-MENSAGEM
              PERFORM 3000-FINALIZAR
           END-IF.

      *----------------------------------------------------------------*
       2400-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *    REALIZA EXCLUSAO DA SOLICITACAO                             *
      ******************************************************************
      *----------------------------------------------------------------*
       2500-EXCLUIR-SOLICITACAO        SECTION.
      *----------------------------------------------------------------*

           MOVE 07                     TO WRK-LOCAL-EIBRESP
                                          WRK-LOCAL-ABCODE.

           MOVE PARM-CNPJ-REST-CH      TO SPARM-CNPJ-REST-CH

           EXEC CICS
                 DELETE DATASET ('SPCNPJRE')
                         RIDFLD (SPARM-CNPJ-REST-CH)
                       NOHANDLE
           END-EXEC.

           IF EIBRESP                  NOT EQUAL DFHRESP(NORMAL) AND
                                                 DFHRESP(NOTFND)
              PERFORM 9998-TRATAR-ERRO
           END-IF.

           IF EIBRESP                  EQUAL DFHRESP(NOTFND)
              MOVE 99                  TO WRK-COMM-RC
              MOVE WRK-MSG-VSAM-04     TO WRK-MENSAGEM
              PERFORM 3000-FINALIZAR
           END-IF.

      *----------------------------------------------------------------*
       2500-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *    FINALIZACAO                                                 *
      ******************************************************************
      *----------------------------------------------------------------*
       3000-FINALIZAR                  SECTION.
      *----------------------------------------------------------------*

           MOVE PARM-CNPJ-RESTRICAO     TO WRK-COMM-AREA-VSAM.
           MOVE WRK-MENSAGEM            TO WRK-COMM-MENSAGEM.
           MOVE WRK-COMMAREA            TO DFHCOMMAREA(1:EIBCALEN).

           EXEC CICS
                RETURN
           END-EXEC.

      *----------------------------------------------------------------*
       3000-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *    TRATAMENTO DE FALHAS                                        *
      ******************************************************************
      *----------------------------------------------------------------*
       9998-TRATAR-ERRO                SECTION.
      *----------------------------------------------------------------*

           IF EIBRESP                  NOT EQUAL ZEROS
              MOVE EIBRESP             TO WRK-EIBRESP
              MOVE WRK-MSG99           TO WRK-MENSAGEM
           ELSE
              PERFORM 9999-ERRO-ABCODE
              MOVE WRK-MSG00           TO WRK-MENSAGEM
           END-IF.

           MOVE 99                     TO WRK-COMM-RC
           PERFORM 3000-FINALIZAR.

      *----------------------------------------------------------------*
       9998-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *    OBTER ABCODE DA FALHA CICS                                  *
      ******************************************************************
      *----------------------------------------------------------------*
       9999-ERRO-ABCODE                SECTION.
      *----------------------------------------------------------------*

           EXEC CICS
                ASSIGN ABCODE(WRK-ABCODE-MSG)
           END-EXEC.

      *----------------------------------------------------------------*
       9999-99-FIM.                    EXIT.
      *----------------------------------------------------------------*
