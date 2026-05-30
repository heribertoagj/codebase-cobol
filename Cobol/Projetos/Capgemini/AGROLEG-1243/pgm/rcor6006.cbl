      *================================================================*
       IDENTIFICATION                  DIVISION.
      *================================================================*
       PROGRAM-ID. RCOR6006.
       AUTHOR.     PRIME INFORMATICA.
      *================================================================*
      *                 P R I M E   I N F O R M A T I C A              *
      *----------------------------------------------------------------*
      *    PROGRAMA....: RCOR6006                                      *
      *    PROGRAMADOR.: PRIME INFORMATICA        - PRIME              *
      *    ANALISTA....: PRIME INFORMATICA        - PRIME              *
      *    DATA........: AGO/2012                                      *
      *----------------------------------------------------------------*
      *    OBJETIVO....: MODULO RESPONSAVEL POR EXECUTAR OS COMANDOS   *
      *                  MQ ATRAVES DA CHAMADA A ROTINA SBAT3510.      *
      *----------------------------------------------------------------*
      *    BOOKS.......:                                               *
      *    I#RCOR11 - AREA DE COMUNICACAO RCOR6006                     *
      *    I#POOLB6 - AREA DE TRATAMENTO DE ERRO - CDES                *
      *    I#SBATMV - AREA DE COMUNICACAO COM MODULO SBAT3510          *
      *----------------------------------------------------------------*
      *    MODULOS.....:                                               *
      *    SBAT3510 - EXECUTA COMANDOS MQ (OPEN/GET/CLOSE)             *
      *================================================================*

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
       01  FILLER                      PIC X(32)           VALUE
           '*  INICIO DA WORKING RCOR6006  *'.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       01  FILLER                      PIC X(32)           VALUE
           '* AREA VARIAVEIS AUXILIARES   *'.
      *----------------------------------------------------------------*
       01  WRK-S9-08                   PIC +9(08)          VALUE ZEROS.
       01  FILLER                      REDEFINES WRK-S9-08.
           05 FILLER                   PIC X(01).
           05 FILLER                   PIC 9(04).
           05 WRK-9-04                 PIC 9(04).
       01  WRK-INVOKING-PGM            PIC X(08)           VALUE SPACES.
       01  WRK-REASON-CODE-X.
           05 WRK-REASON-CODE          PIC 9(08)           VALUE ZEROS.

       01  WRK-RESTO-COMP              PIC 9(002) COMP-3   VALUE ZEROS.
       01  WRK-RESTO                   PIC 9(002)          VALUE ZEROS.
           88  INDICE-PAR              VALUE ZERO.
           88  INDICE-IMPAR            VALUE 1.

       01  WRK-LENGTH                  PIC 9(05) COMP      VALUE ZEROS.

       01  WRK-SBAT.
           03  WRK-AREA-MCA            PIC X(252)          VALUE SPACES.
           03  WRK-TAMANHO             PIC S9(008) COMP    VALUE ZEROS.
           03  WRK-MENSAGEM            PIC X(32500)        VALUE SPACES.

       01  WRK-ERR-MQ.
           05 WRK-ERR-MQ-FILA          PIC  X(40)          VALUE SPACES.
           05 FILLER                   PIC  X(01)          VALUE '/'.
           05 WRK-ERR-MQ-FUNCAO        PIC  X(02)          VALUE SPACES.
           05 FILLER                   PIC  X(07)          VALUE
              '/COUNT:'.
           05 WRK-ERR-MQ-COUNT         PIC  ZZ9            VALUE ZEROS.
           05 FILLER                   PIC  X(01)          VALUE '/'.
           05 WRK-ERR-MQ-REASON        PIC  ZZZZZZZ9       VALUE ZEROS.

       01  WRK-ERRO-CICS.
           03  FILLER                  PIC  X(08)          VALUE
               'LOCAL = '.
           03  WRK-LOCAL-ERRO          PIC  9(03)          VALUE ZEROS.
           03  FILLER                  PIC  X(01)          VALUE SPACES.
           03  FILLER                  PIC  X(10)          VALUE
               'EIBRESP = '.
           03  WRK-EIBRESP             PIC  9(04)          VALUE ZEROS.
           03  FILLER                  PIC  X(01)          VALUE SPACES.
           03  WRK-TEXTO-ERRO          PIC  X(80)          VALUE SPACES.


       01  WRK-MSG-ERRO.
           03  WRK-MSG001              PIC  X(80)          VALUE
              'ERRO NO ACESSO AO MODULO SBAT3510 - FUNCAO OPEN'.
           03  WRK-MSG002              PIC  X(80)          VALUE
              'ERRO NO ACESSO AO MODULO SBAT3510 - FUNCAO CLOSE'.
           03  WRK-MSG003              PIC  X(80)          VALUE
              'ERRO NO ACESSO AO MODULO SBAT3510 - FUNCAO GET'.
           03  WRK-MSG004              PIC  X(80)          VALUE
              'ERRO NO ACESSO AO MODULO SBAT3510 - FUNCAO PUT'.

      *----------------------------------------------------------------*
       01  FILLER                      PIC X(32)           VALUE
           '* AREA CONVERSAO HEXADECIMAL   *'.
      *----------------------------------------------------------------*

       01  WRK-LINHA-CARAC             PIC  X(024)        VALUE SPACES.
       01  WRK-LINHA-HEXA-ACIMA        PIC  X(024)        VALUE SPACES.
       01  WRK-LINHA-HEXA-ABAIXO       PIC  X(024)        VALUE SPACES.
       01  WRK-INDICE                  PIC  9(003) COMP-3 VALUE ZEROS.
       01  WRK-RESULT                  PIC  9(003) COMP-3 VALUE ZEROS.
       01  WRK-POS-ABAIXO              PIC  9(003) COMP-3 VALUE ZEROS.
       01  WRK-POS-ACIMA               PIC  9(003) COMP-3 VALUE ZEROS.

       01  FILLER.
           COPY 'I#RCOR12'.

      *----------------------------------------------------------------*
       01  FILLER                      PIC X(32)           VALUE
           '* AREA DE COMUNICACAO SBAT3510 *'.
      *----------------------------------------------------------------*

       COPY 'I#SBATMV'.

      *----------------------------------------------------------------*
       01  FILLER                      PIC X(32)           VALUE
           '* AREA PARA ERRO CICS / DB2    *'.
      *----------------------------------------------------------------*

       COPY 'I#POOLB6'.

      *----------------------------------------------------------------*
       01  FILLER                      PIC X(32)           VALUE
           '* DFHCOMMAREA *'.
      *----------------------------------------------------------------*

       COPY 'I#RCOR11'.

      *----------------------------------------------------------------*
       01  FILLER                      PIC X(32)           VALUE
           '*   FIM DA WORKING RCOR6006    *'.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       LINKAGE                         SECTION.
      *----------------------------------------------------------------*

       01  DFHCOMMAREA                 PIC X(30429).

      *================================================================*
       PROCEDURE                       DIVISION USING DFHCOMMAREA.
      *================================================================*

      *----------------------------------------------------------------*
       0000-ROTINA-PRINCIPAL           SECTION.
      *----------------------------------------------------------------*

           EXEC CICS HANDLE ABEND
               LABEL (9000-FORMATAR-ERRO-CICS)
           END-EXEC.

           IF (EIBRESP                 EQUAL DFHRESP(NORMAL))
               CONTINUE
           END-IF.

           MOVE DFHCOMMAREA(1:EIBCALEN)
                                       TO RCOR11-REGISTRO.

           PERFORM 1000-PROCESSAR.

           PERFORM 2000-RETORNAR.

      *----------------------------------------------------------------*
       0000-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       1000-PROCESSAR                  SECTION.
      *----------------------------------------------------------------*

           INITIALIZE RCOR11-RETORNO.

           IF (RCOR11-A-FUNCAO         NOT EQUAL 'O'  AND 'C' AND 'G'
                                            AND  'GB' AND 'P' AND 'PU')
               MOVE 1                  TO RCOR11-S-CD-RETORNO
               GO                      TO 1000-99-FIM
           END-IF.

           IF (RCOR11-A-FILA           EQUAL SPACES)
               MOVE 2                  TO RCOR11-S-CD-RETORNO
               GO                      TO 1000-99-FIM
           END-IF.

           EVALUATE RCOR11-A-FUNCAO
               WHEN 'O'  PERFORM 1100-ABRIR-FILA-PESQUISA
               WHEN 'C'  PERFORM 1200-FECHAR-FILA
               WHEN 'G'  PERFORM 1300-PEGAR-MSG-REMOVENDO
               WHEN 'GB' PERFORM 1400-PEGAR-MSG-SEM-REMOVER
               WHEN 'P'  PERFORM 1500-INSERIR-MSG
               WHEN 'PU' PERFORM 1600-INSERIR-MSG-UNICA
           END-EVALUATE.

      *----------------------------------------------------------------*
       1000-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       1100-ABRIR-FILA-PESQUISA        SECTION.
      *----------------------------------------------------------------*

           MOVE SPACES                 TO MQ-MCA.

           INITIALIZE MQ-MCA.

           MOVE 'OPEN'                 TO MQ-FUNCAO.
           MOVE 'GET'                  TO MQ-OPCAO.
           MOVE RCOR11-A-FILA          TO MQ-FILA.

           EXEC CICS LINK
               PROGRAM ('SBAT3510')
               COMMAREA(MQ-MCA)
               LENGTH  (LENGTH OF MQ-MCA)
               NOHANDLE
           END-EXEC.

           IF (EIBRESP                 NOT EQUAL ZEROS)
               MOVE WRK-MSG001         TO WRK-TEXTO-ERRO
               MOVE EIBRESP            TO WRK-EIBRESP
               MOVE 10                 TO WRK-LOCAL-ERRO
               PERFORM 9000-FORMATAR-ERRO-CICS
           ELSE
              IF (MQ-COMPLETION-CODE   NOT EQUAL ZEROS)
                  PERFORM 9200-GRAVAR-LOG-MQ
              ELSE
                  MOVE MQ-HANDLE-FILA  TO RCOR11-A-HANDLE-FILA
              END-IF
           END-IF.

      *----------------------------------------------------------------*
       1100-99-FIM.                    EXIT.
      *----------------------------------------------------------------*
      *----------------------------------------------------------------*
       1200-FECHAR-FILA                SECTION.
      *----------------------------------------------------------------*

           MOVE SPACES                 TO MQ-MCA.

           INITIALIZE MQ-MCA.

           MOVE 'CLOSE'                TO MQ-FUNCAO.
           MOVE SPACES                 TO MQ-OPCAO.
           MOVE RCOR11-A-FILA          TO MQ-FILA.
           MOVE RCOR11-A-HANDLE-FILA   TO MQ-HANDLE-FILA.

           EXEC CICS LINK
               PROGRAM ('SBAT3510')
               COMMAREA(MQ-MCA)
               LENGTH  (LENGTH OF MQ-MCA)
               NOHANDLE
           END-EXEC.

           IF (EIBRESP                 NOT EQUAL ZEROS)
               MOVE WRK-MSG002         TO WRK-TEXTO-ERRO
               MOVE EIBRESP            TO WRK-EIBRESP
               MOVE 20                 TO WRK-LOCAL-ERRO
               PERFORM 9000-FORMATAR-ERRO-CICS
           ELSE
              IF (MQ-COMPLETION-CODE   NOT EQUAL ZEROS)
                  PERFORM 9200-GRAVAR-LOG-MQ
              END-IF
           END-IF.

      *----------------------------------------------------------------*
       1200-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       1300-PEGAR-MSG-REMOVENDO        SECTION.
      *----------------------------------------------------------------*

           MOVE SPACES                 TO MQ-MCA.

           INITIALIZE MQ-MCA.

           MOVE 3                      TO MQ-FORMATO.
           MOVE 'GET'                  TO MQ-FUNCAO.
           MOVE SPACES                 TO MQ-OPCAO.
           MOVE RCOR11-A-HANDLE-FILA   TO MQ-HANDLE-FILA.
           MOVE RCOR11-A-FILA          TO MQ-FILA.
           MOVE SPACES                 TO MQ-FILA-RESPOSTA.
           MOVE ZEROS                  TO MQ-PRIORIDADE-MSG.
           MOVE ZEROS                  TO MQ-TEMPO-ESPERA.
           MOVE ZEROS                  TO MQ-NOTIFICACAO.
           MOVE 8                      TO MQ-TIPO-MENSAGEM.
           MOVE 16384                  TO MQ-OPCOES-DE-GET.
7C2511     ADD 64                      TO MQ-OPCOES-DE-GET.
           MOVE SPACES                 TO MQ-ID-MENSAGEM
                                          MQ-ID-DE-CORRELACAO.
           MOVE MQ-MCA                 TO WRK-AREA-MCA.
           MOVE LENGTH OF WRK-MENSAGEM TO WRK-TAMANHO.

           EXEC CICS LINK
               PROGRAM ('SBAT3510')
               COMMAREA(WRK-SBAT)
               LENGTH  (LENGTH OF WRK-SBAT)
               NOHANDLE
           END-EXEC.

           IF (EIBRESP                 NOT EQUAL ZEROS)  OR
7C2511*       (MQ-REASON-CODE          NOT EQUAL 0 AND 2033 AND 2080)
7C2511        (MQ-REASON-CODE          NOT EQUAL 0 AND 2033 AND 2079)
               MOVE WRK-MSG003         TO WRK-TEXTO-ERRO
               MOVE EIBRESP            TO WRK-EIBRESP
               MOVE 30                 TO WRK-LOCAL-ERRO
               PERFORM 9000-FORMATAR-ERRO-CICS
           END-IF.

           MOVE WRK-AREA-MCA           TO MQ-MCA
           MOVE MQ-REASON-CODE         TO WRK-S9-08
           MOVE WRK-9-04               TO RCOR11-S-REASON-CODE

7C2511*
7C2511*    TRATAMENTO PARA PERMITIR MENSAGEM TRUNCADA
7C2511     IF  MQ-COMPLETION-CODE      EQUAL 1
7C2511     AND MQ-REASON-CODE          EQUAL 2079
7C2511         MOVE ZEROS              TO MQ-COMPLETION-CODE
7C2511     END-IF

           IF  (MQ-COMPLETION-CODE     NOT EQUAL ZEROS)
               IF (MQ-REASON-CODE      NOT EQUAL 2033)
                  PERFORM 9200-GRAVAR-LOG-MQ
                  MOVE 99              TO RCOR11-S-CD-RETORNO
                  GO                   TO 1300-99-FIM
               END-IF
               MOVE 100                TO RCOR11-S-CD-RETORNO
           ELSE
7C2511         IF (MQ-REASON-CODE      EQUAL 2079)
7C2511            PERFORM 9200-GRAVAR-LOG-MQ
7C2511         END-IF

               IF MQ-CONTA-BACKOUT     GREATER 3
                  PERFORM 9200-GRAVAR-LOG-MQ
                  MOVE 100             TO RCOR11-S-CD-RETORNO
                  GO                   TO 1300-99-FIM
               END-IF

               MOVE WRK-TAMANHO        TO RCOR11-A-TAMANHO
               MOVE WRK-MENSAGEM       TO RCOR11-A-MENSAGEM
               MOVE MQ-ID-MENSAGEM     TO WRK-LINHA-CARAC
               PERFORM 1610-CONVERTER-HEXA
           END-IF.

      *----------------------------------------------------------------*
       1300-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       1400-PEGAR-MSG-SEM-REMOVER      SECTION.
      *----------------------------------------------------------------*

           MOVE SPACES                 TO MQ-MCA.

           INITIALIZE MQ-MCA.

           MOVE 3                      TO MQ-FORMATO.
           MOVE 'GET'                  TO MQ-FUNCAO.
           MOVE 'BROWSE'               TO MQ-OPCAO.
           MOVE RCOR11-A-HANDLE-FILA   TO MQ-HANDLE-FILA.
           MOVE RCOR11-A-FILA          TO MQ-FILA.
           MOVE SPACES                 TO MQ-FILA-RESPOSTA.
           MOVE ZEROS                  TO MQ-PRIORIDADE-MSG.
           MOVE ZEROS                  TO MQ-TEMPO-ESPERA.
           MOVE ZEROS                  TO MQ-NOTIFICACAO.
           MOVE 8                      TO MQ-TIPO-MENSAGEM.
           MOVE ZEROS                  TO MQ-OPCOES-DE-GET.
           MOVE SPACES                 TO MQ-ID-MENSAGEM
                                          MQ-ID-DE-CORRELACAO.
           MOVE MQ-MCA                 TO WRK-AREA-MCA.
           MOVE LENGTH OF WRK-MENSAGEM TO WRK-TAMANHO.

           EXEC CICS LINK
               PROGRAM ('SBAT3510')
               COMMAREA(WRK-SBAT)
               LENGTH  (LENGTH OF WRK-SBAT)
               NOHANDLE
           END-EXEC.

           IF (EIBRESP                 NOT EQUAL ZEROS)  OR
              (MQ-REASON-CODE          NOT EQUAL 0 AND 2033 AND 2080)
               MOVE WRK-MSG003         TO WRK-TEXTO-ERRO
               MOVE EIBRESP            TO WRK-EIBRESP
               MOVE 40                 TO WRK-LOCAL-ERRO
               PERFORM 9000-FORMATAR-ERRO-CICS
           END-IF.

           MOVE WRK-AREA-MCA           TO MQ-MCA
           MOVE MQ-REASON-CODE         TO WRK-S9-08
           MOVE WRK-9-04               TO RCOR11-S-REASON-CODE

           IF  (MQ-COMPLETION-CODE     NOT EQUAL ZEROS)
               IF (MQ-REASON-CODE      NOT EQUAL 2033)
                  PERFORM 9200-GRAVAR-LOG-MQ
               END-IF

               MOVE 100                TO RCOR11-S-CD-RETORNO
           ELSE
               IF MQ-CONTA-BACKOUT     GREATER 3
                  PERFORM 9200-GRAVAR-LOG-MQ
                  MOVE 100             TO RCOR11-S-CD-RETORNO
                  GO                   TO 1400-99-FIM
               END-IF
               MOVE WRK-TAMANHO        TO RCOR11-A-TAMANHO
               MOVE WRK-MENSAGEM       TO RCOR11-A-MENSAGEM
               MOVE MQ-ID-MENSAGEM     TO WRK-LINHA-CARAC
               PERFORM 1610-CONVERTER-HEXA
           END-IF.

      *----------------------------------------------------------------*
       1400-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       1500-INSERIR-MSG                SECTION.
      *----------------------------------------------------------------*

           MOVE SPACES                 TO MQ-MCA.

           INITIALIZE MQ-MCA.

           MOVE 1                      TO MQ-FORMATO.
           MOVE 'PUT'                  TO MQ-FUNCAO.
           MOVE SPACES                 TO MQ-OPCAO.
           MOVE RCOR11-A-FILA          TO MQ-FILA.
           MOVE ZEROS                  TO MQ-PRIORIDADE-MSG.
           MOVE 8                      TO MQ-TIPO-MENSAGEM.
           MOVE 2                      TO MQ-OPCOES-DE-PUT.
           MOVE 'N'                    TO MQ-FORCA-IDS.
           MOVE MQ-MCA                 TO WRK-AREA-MCA.
           MOVE RCOR11-A-TAMANHO       TO WRK-TAMANHO.
           MOVE RCOR11-A-MENSAGEM      TO WRK-MENSAGEM.

           COMPUTE WRK-LENGTH          = RCOR11-A-TAMANHO
                                       + LENGTH OF WRK-AREA-MCA
                                       + LENGTH OF WRK-TAMANHO.

           EXEC CICS LINK
               PROGRAM ('SBAT3510')
               COMMAREA(WRK-SBAT)
               LENGTH  (LENGTH OF WRK-SBAT)
               NOHANDLE
           END-EXEC.

           IF (EIBRESP                 NOT EQUAL ZEROS)  OR
              (MQ-COMPLETION-CODE      NOT EQUAL ZEROS)
               MOVE WRK-MSG004         TO WRK-TEXTO-ERRO
               MOVE EIBRESP            TO WRK-EIBRESP
               MOVE 50                 TO WRK-LOCAL-ERRO
               PERFORM 9000-FORMATAR-ERRO-CICS
           END-IF.

           MOVE WRK-AREA-MCA           TO MQ-MCA
           MOVE MQ-REASON-CODE         TO WRK-S9-08
           MOVE WRK-9-04               TO RCOR11-S-REASON-CODE

           IF  (MQ-COMPLETION-CODE     NOT EQUAL ZEROS)
               PERFORM 9200-GRAVAR-LOG-MQ
               MOVE 99                 TO RCOR11-S-CD-RETORNO
           ELSE
               MOVE MQ-ID-MENSAGEM     TO WRK-LINHA-CARAC
               PERFORM 1610-CONVERTER-HEXA
           END-IF.

      *----------------------------------------------------------------*
       1500-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       1600-INSERIR-MSG-UNICA          SECTION.
      *----------------------------------------------------------------*

           MOVE SPACES                 TO MQ-MCA.

           INITIALIZE MQ-MCA.

           MOVE 1                      TO MQ-FORMATO.
           MOVE 'PUT'                  TO MQ-FUNCAO.
           MOVE 'UNICO'                TO MQ-OPCAO.
           MOVE RCOR11-A-FILA          TO MQ-FILA.
           MOVE ZEROS                  TO MQ-PRIORIDADE-MSG.
           MOVE 8                      TO MQ-TIPO-MENSAGEM.
           MOVE 2                      TO MQ-OPCOES-DE-PUT.
           MOVE 'N'                    TO MQ-FORCA-IDS.
           MOVE MQ-MCA                 TO WRK-AREA-MCA.
           MOVE RCOR11-A-TAMANHO       TO WRK-TAMANHO.
           MOVE RCOR11-A-MENSAGEM      TO WRK-MENSAGEM.

           COMPUTE WRK-LENGTH          = RCOR11-A-TAMANHO
                                       + LENGTH OF WRK-AREA-MCA
                                       + LENGTH OF WRK-TAMANHO.

           EXEC CICS LINK
               PROGRAM ('SBAT3510')
               COMMAREA(WRK-SBAT)
               LENGTH  (LENGTH OF WRK-SBAT)
               NOHANDLE
           END-EXEC.

           IF (EIBRESP                 NOT EQUAL ZEROS)  OR
              (MQ-COMPLETION-CODE      NOT EQUAL ZEROS)
               MOVE WRK-MSG004         TO WRK-TEXTO-ERRO
               MOVE EIBRESP            TO WRK-EIBRESP
               MOVE 60                 TO WRK-LOCAL-ERRO
               PERFORM 9000-FORMATAR-ERRO-CICS
           END-IF.

           MOVE WRK-AREA-MCA           TO MQ-MCA
           MOVE MQ-REASON-CODE         TO WRK-S9-08
           MOVE WRK-9-04               TO RCOR11-S-REASON-CODE

           IF  (MQ-COMPLETION-CODE     NOT EQUAL ZEROS)
               PERFORM 9200-GRAVAR-LOG-MQ
               MOVE 99                 TO RCOR11-S-CD-RETORNO
           ELSE
               MOVE MQ-ID-MENSAGEM     TO WRK-LINHA-CARAC
               PERFORM 1610-CONVERTER-HEXA
           END-IF.

      *----------------------------------------------------------------*
       1600-99-FIM.                    EXIT.
      *----------------------------------------------------------------*
      *----------------------------------------------------------------*
       1610-CONVERTER-HEXA             SECTION.
      *----------------------------------------------------------------*

           MOVE SPACES                 TO WRK-LINHA-HEXA-ACIMA
                                          WRK-LINHA-HEXA-ABAIXO.
           MOVE WRK-LINHA-CARAC        TO HEXBC-DATA.
           MOVE 24                     TO HEXBC-DATA-LENGTH.

           SET HEXBC-FUNCTION-HEXA-TO-CHAR
                                       TO TRUE.

           MOVE ZEROS                  TO HEXBC-RETURN-CODE
                                          HEXBC-REASON-CODE.

           EXEC CICS LINK
                  PROGRAM ('SBIHEXBC')
                 COMMAREA (COMMHEXBC-AREA)
           END-EXEC.

           IF (EIBRESP                 EQUAL ZEROS)
               CONTINUE
           END-IF.

           IF HEXBC-RC-OK
              PERFORM 1611-MONTAR-LINHAS
              MOVE WRK-LINHA-HEXA-ACIMA
                                       TO RCOR11-A-MQID(01:24)
              MOVE WRK-LINHA-HEXA-ABAIXO
                                       TO RCOR11-A-MQID(25:24)
           END-IF.

      *----------------------------------------------------------------*
       1610-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       1611-MONTAR-LINHAS              SECTION.
      *----------------------------------------------------------------*

           MOVE 1                      TO WRK-POS-ABAIXO
                                          WRK-POS-ACIMA
                                          WRK-INDICE.

           PERFORM UNTIL WRK-INDICE    GREATER 48

               DIVIDE WRK-INDICE BY 2  GIVING    WRK-RESULT
                                       REMAINDER WRK-RESTO-COMP

               MOVE WRK-RESTO-COMP     TO WRK-RESTO

               EVALUATE TRUE
                   WHEN INDICE-PAR
                        MOVE HEXBC-DATA(WRK-INDICE:1)
                             TO WRK-LINHA-HEXA-ABAIXO(WRK-POS-ABAIXO:1)
                        ADD  1      TO WRK-POS-ABAIXO
                   WHEN INDICE-IMPAR
                        MOVE HEXBC-DATA(WRK-INDICE:1)
                             TO WRK-LINHA-HEXA-ACIMA(WRK-POS-ACIMA:1)
                        ADD  1      TO WRK-POS-ACIMA
               END-EVALUATE

               ADD 1                TO WRK-INDICE

           END-PERFORM.

      *----------------------------------------------------------------*
       1611-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       2000-RETORNAR                   SECTION.
      *----------------------------------------------------------------*

           MOVE RCOR11-REGISTRO        TO DFHCOMMAREA(1:EIBCALEN).

           EXEC CICS
               RETURN
           END-EXEC.

      *----------------------------------------------------------------*
       2000-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       9000-FORMATAR-ERRO-CICS         SECTION.
      *----------------------------------------------------------------*

           MOVE 'VRS001'               TO ERR-VERSAO.
           MOVE ZEROS                  TO ERR-COD-MSG.
           MOVE SPACES                 TO ERR-TEXTO-MSG.
           MOVE ZEROS                  TO ERR-RETURN-CODE.
           MOVE 'APL'                  TO ERR-TIPO-ACESSO.
           MOVE 'RCOR6006'             TO ERR-PGM.
           MOVE WRK-ERRO-CICS          TO ERR-TEXTO.
           MOVE 'RCOR6000'             TO ERR-MODULO.
           MOVE DFHEIBLK               TO ERR-DFHEIBLK.
           MOVE 99                     TO RCOR11-S-CD-RETORNO.

           PERFORM 9100-GRAVAR-LOG-CDES.

      *----------------------------------------------------------------*
       9000-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       9100-GRAVAR-LOG-CDES            SECTION.
      *----------------------------------------------------------------*

           MOVE 'VRS001'               TO ERR-VERSAO.

           EXEC CICS ASSIGN
                     APPLID(ERR-CICS)
           END-EXEC.


           EXEC CICS START
               TRANSID('CD11')
               FROM   (ERRO-AREA)
               LENGTH (+520)
               NOHANDLE
           END-EXEC.

           IF (EIBRESP                 EQUAL DFHRESP(NORMAL))
               CONTINUE
           END-IF.

      *----------------------------------------------------------------*
       9100-99-FIM.                    EXIT.
      *----------------------------------------------------------------*
      *----------------------------------------------------------------*
       9200-GRAVAR-LOG-MQ              SECTION.
      *----------------------------------------------------------------*

           EXEC CICS ASSIGN INVOKINGPROG (WRK-INVOKING-PGM)
           END-EXEC.

           MOVE ZEROS                  TO ERR-COD-MSG.
           MOVE MQ-FILA                TO WRK-ERR-MQ-FILA
           MOVE RCOR11-A-FUNCAO        TO WRK-ERR-MQ-FUNCAO
           MOVE MQ-REASON-CODE         TO WRK-REASON-CODE
           MOVE WRK-REASON-CODE        TO WRK-ERR-MQ-REASON
           MOVE MQ-CONTA-BACKOUT       TO WRK-ERR-MQ-COUNT
           MOVE WRK-ERR-MQ             TO ERR-TEXTO-MSG.
           MOVE ZEROS                  TO ERR-RETURN-CODE.
           MOVE 'APL'                  TO ERR-TIPO-ACESSO.
           MOVE 'RCOR6006'             TO ERR-PGM.
           MOVE WRK-INVOKING-PGM       TO ERR-MODULO.

           PERFORM 9100-GRAVAR-LOG-CDES.

      *----------------------------------------------------------------*
       9200-99-FIM.                    EXIT.
      *----------------------------------------------------------------*
