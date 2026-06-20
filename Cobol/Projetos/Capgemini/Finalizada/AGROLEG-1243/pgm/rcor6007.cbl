      *----------------------------------------------------------------*
       PROCESS TRUNC(BIN)
      *================================================================*
       IDENTIFICATION                  DIVISION.
      *================================================================*
       PROGRAM-ID. RCOR6007.
       AUTHOR.     PRIME INFORMATICA.
      *================================================================*
      *                 P R I M E   I N F O R M A T I C A              *
      *----------------------------------------------------------------*
      *    PROGRAMA....: RCOR6007                                      *
      *    PROGRAMADOR.: PRIME INFORMATICA        - PRIME              *
      *    ANALISTA....: PRIME INFORMATICA        - PRIME              *
      *    DATA........: MAIO/2026                                     *
      *----------------------------------------------------------------*
      *    OBJETIVO....: MODULO RESPONSAVEL POR EXECUTAR OPEN/GET/     *
      *                  CLOSE MQ VIA SBAT3520, COM BUFFER AMPLIADO     *
      *                  PARA NORMALIZAR WKTPART DA COR0001E ANTES     *
      *                  DE RETORNAR AO FLUXO RCOR.                    *
      *----------------------------------------------------------------*
      *    BOOKS.......:                                               *
      *    I#RCOR11 - AREA DE COMUNICACAO RCOR6007                     *
      *    I#POOLB6 - AREA DE TRATAMENTO DE ERRO - CDES                *
      *    SBATWC01 - AREA DE COMUNICACAO COM MODULO SBAT3520          *
      *----------------------------------------------------------------*
      *    MODULOS.....:                                               *
      *    SBAT3520 - EXECUTA COMANDOS MQ COM BUFFER POR PONTEIRO      *
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
           '*  INICIO DA WORKING RCOR6007  *'.
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

       01  WRK-SBAT3520                PIC X(08)           VALUE
           'SBAT3520'.
       01  WRK-TAM-CAND                PIC S9(08) COMP     VALUE ZEROS.
       01  WRK-TAM-MSG-LIDO            PIC S9(08) COMP     VALUE ZEROS.
       01  WRK-TAM-MSG-NORM            PIC S9(08) COMP     VALUE ZEROS.

       01  WRK-PTR-BUFFER              USAGE IS POINTER    VALUE NULL.
       01  FILLER                      REDEFINES WRK-PTR-BUFFER.
           05 WRK-PTR-BUFFER-COMP      PIC 9(09) COMP.

       01  WRK-MQ-MSGEM.
           05 FILLER                   PIC X(008)           VALUE
              'SBAT3520'.
           05 WRK-MQ-MSGEM-AREA        PIC X(300000)       VALUE SPACES.
           05 WRK-MQ-LIT-FIM           PIC X(008)          VALUE SPACES.

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
              'ERRO NO ACESSO AO MODULO SBAT3520 - FUNCAO OPEN'.
           03  WRK-MSG002              PIC  X(80)          VALUE
              'ERRO NO ACESSO AO MODULO SBAT3520 - FUNCAO CLOSE'.
           03  WRK-MSG003              PIC  X(80)          VALUE
              'ERRO NO ACESSO AO MODULO SBAT3520 - FUNCAO GET'.
           03  WRK-MSG004              PIC  X(80)          VALUE
              'COR0001E NORMALIZADA EXCEDE 30000 BYTES'.

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
           '* AREA COMUNICACAO SBAT3520    *'.
      *----------------------------------------------------------------*

           COPY 'SBATWC01'.

      *----------------------------------------------------------------*
       01  FILLER                      PIC X(32)           VALUE
           '* AREA COMPACTACAO WKTPART     *'.
      *----------------------------------------------------------------*
       01  WRK-WKT-SCAN-DES            PIC S9(08) COMP     VALUE ZEROS.
       01  WRK-WKT-I                   PIC S9(08) COMP     VALUE ZEROS.
       01  WRK-WKT-J                   PIC S9(08) COMP     VALUE ZEROS.
       01  WRK-WKT-CSTART              PIC S9(08) COMP     VALUE ZEROS.
       01  WRK-WKT-HIT                 PIC X(01)           VALUE SPACES.
       01  WRK-WKT-CONTINUA            PIC X(01)           VALUE SPACES.
       01  WRK-NEXT-BRA                PIC S9(08) COMP     VALUE ZEROS.
       01  WRK-WKT-IEND                PIC S9(08) COMP     VALUE ZEROS.
       01  WRK-WKT-OLD-LEN             PIC S9(08) COMP     VALUE ZEROS.
       01  WRK-WKT-NEW-LEN             PIC S9(08) COMP     VALUE ZEROS.
       01  WRK-WKT-DELTA               PIC S9(08) COMP     VALUE ZEROS.
       01  WRK-WKT-TAIL-L              PIC S9(08) COMP     VALUE ZEROS.
       01  WRK-WKT-OUT-IX              PIC S9(08) COMP     VALUE ZEROS.
       01  WRK-CH-WKT                  PIC X(01)           VALUE SPACES.
       01  WRK-WKT-PREV-SP             PIC X(01)           VALUE SPACES.
       01  WRK-WKT-BUF                 PIC X(10000)       VALUE SPACES.
       01  WRK-OVERLAP-MOV             PIC X(300000)       VALUE SPACES.

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
           '*   FIM DA WORKING RCOR6007    *'.
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

           IF (RCOR11-A-FUNCAO         NOT EQUAL 'O'  AND 'C' AND 'G')
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
           END-EVALUATE.

      *----------------------------------------------------------------*
       1000-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       1100-ABRIR-FILA-PESQUISA        SECTION.
      *----------------------------------------------------------------*

           MOVE SPACES                 TO MQ-MCA.

           INITIALIZE MQ-MCA.

           MOVE 1                      TO MQ-FORMATO.
           MOVE 'OPEN'                 TO MQ-FUNCAO.
           MOVE 'GET'                  TO MQ-OPCAO.
           MOVE RCOR11-A-FILA          TO MQ-FILA.

           EXEC CICS LINK
               PROGRAM ('SBAT3520')
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
                  MOVE 99              TO RCOR11-S-CD-RETORNO
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
               PROGRAM ('SBAT3520')
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
                  MOVE 99              TO RCOR11-S-CD-RETORNO
              END-IF
           END-IF.

      *----------------------------------------------------------------*
       1200-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       1300-PEGAR-MSG-REMOVENDO        SECTION.
      *----------------------------------------------------------------*

           MOVE SPACES                 TO MQ-MCA
                                          WRK-MQ-MSGEM-AREA
                                          RCOR11-A-MENSAGEM.

           INITIALIZE MQ-MCA.

           MOVE WRK-SBAT3520           TO WRK-MQ-LIT-FIM.

           SET WRK-PTR-BUFFER          TO ADDRESS
                                       OF WRK-MQ-MSGEM.

           MOVE 3                      TO MQ-FORMATO.
           MOVE 'GET'                  TO MQ-FUNCAO.
           MOVE SPACES                 TO MQ-OPCAO.
           MOVE RCOR11-A-HANDLE-FILA   TO MQ-HANDLE-FILA.
           MOVE RCOR11-A-FILA          TO MQ-FILA.
           MOVE SPACES                 TO MQ-FILA-RESPOSTA
                                          MQ-QM-CONEXAO
                                          MQ-QM-RESPOSTA.
           MOVE ZEROS                  TO MQ-PRIORIDADE-MSG.
           MOVE ZEROS                  TO MQ-TEMPO-ESPERA.
           MOVE ZEROS                  TO MQ-NOTIFICACAO.
           MOVE 8                      TO MQ-TIPO-MENSAGEM.
           MOVE 16384                  TO MQ-OPCOES-DE-GET.
           ADD 64                      TO MQ-OPCOES-DE-GET.
           MOVE SPACES                 TO MQ-ID-MENSAGEM
                                          MQ-ID-DE-CORRELACAO.
           MOVE LENGTH OF WRK-MQ-MSGEM TO MQ-TAMANHO-BUFFER.
           MOVE WRK-PTR-BUFFER-COMP    TO MQ-PTR-BUFFER-COMP.
           MOVE ZEROS                  TO MQ-TAMANHO-BUFFER-LIDO.

           EXEC CICS LINK
               PROGRAM ('SBAT3520')
               COMMAREA(MQ-MCA)
               LENGTH  (LENGTH OF MQ-MCA)
               NOHANDLE
           END-EXEC.

           IF (EIBRESP                 NOT EQUAL ZEROS)
               MOVE WRK-MSG003         TO WRK-TEXTO-ERRO
               MOVE EIBRESP            TO WRK-EIBRESP
               MOVE 30                 TO WRK-LOCAL-ERRO
               PERFORM 9000-FORMATAR-ERRO-CICS
               GO                      TO 1300-90-FIM
           END-IF.

           MOVE MQ-REASON-CODE         TO WRK-S9-08.
           MOVE WRK-9-04               TO RCOR11-S-REASON-CODE.

           IF  (MQ-COMPLETION-CODE     NOT EQUAL ZEROS)
               IF (MQ-REASON-CODE      NOT EQUAL 2033)
                  PERFORM 9200-GRAVAR-LOG-MQ
                  MOVE 99              TO RCOR11-S-CD-RETORNO
                  GO                   TO 1300-90-FIM
               END-IF
               MOVE 100                TO RCOR11-S-CD-RETORNO
               GO                      TO 1300-90-FIM
           END-IF.

           IF MQ-CONTA-BACKOUT         GREATER 3
              PERFORM 9200-GRAVAR-LOG-MQ
              MOVE 100                 TO RCOR11-S-CD-RETORNO
              GO                       TO 1300-90-FIM
           END-IF.

           PERFORM 1310-AJUSTAR-TAMANHO-LIDO.

           IF  WRK-TAM-MSG-LIDO        GREATER +300000
               MOVE WRK-MSG004         TO RCOR11-S-MENSAGEM
               MOVE 98                 TO RCOR11-S-CD-RETORNO
               GO                      TO 1300-90-FIM
           END-IF.

           MOVE WRK-TAM-MSG-LIDO       TO WRK-TAM-MSG-NORM.

CG2605*    IF  WRK-MQ-MSGEM-AREA(1:8)  EQUAL 'COR0001E'
CG2605*        PERFORM 3000-NORMALIZA-WKT-WKTPART
CG2605*    END-IF.
CG2605     PERFORM 3000-NORMALIZA-WKT-WKTPART.

           IF  WRK-TAM-MSG-NORM        GREATER +30000
               MOVE WRK-MSG004         TO RCOR11-S-MENSAGEM
               MOVE 98                 TO RCOR11-S-CD-RETORNO
               GO                      TO 1300-90-FIM
           END-IF.

           MOVE WRK-TAM-MSG-NORM       TO RCOR11-A-TAMANHO.
           MOVE WRK-MQ-MSGEM-AREA(1:WRK-TAM-MSG-NORM)
                                       TO RCOR11-A-MENSAGEM
           MOVE MQ-ID-MENSAGEM         TO WRK-LINHA-CARAC.
           PERFORM 1610-CONVERTER-HEXA.

       1300-90-FIM.

      *----------------------------------------------------------------*
       1300-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       1310-AJUSTAR-TAMANHO-LIDO       SECTION.
      *----------------------------------------------------------------*

           MOVE MQ-TAMANHO-BUFFER-LIDO TO WRK-TAM-MSG-LIDO.

           IF  WRK-TAM-MSG-LIDO        GREATER +16
               COMPUTE WRK-TAM-CAND    = WRK-TAM-MSG-LIDO - 16
               IF  WRK-TAM-CAND        GREATER ZEROS
               AND WRK-TAM-CAND        NOT GREATER +300000
                   COMPUTE WRK-WKT-OUT-IX = WRK-TAM-CAND + 9
                   IF  WRK-MQ-MSGEM(WRK-WKT-OUT-IX:8)
                                       EQUAL WRK-SBAT3520
                       MOVE WRK-TAM-CAND
                                       TO WRK-TAM-MSG-LIDO
                   END-IF
               END-IF
           END-IF.

      *----------------------------------------------------------------*
       1310-99-FIM.                    EXIT.
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
       3000-NORMALIZA-WKT-WKTPART      SECTION.
      *----------------------------------------------------------------*
CG2605*    AGROLEG-1243 - VARRE BUFFER MQ COMPLETO (SIMM03/SIMM04+COR)
CG2605*    BUSCANDO TAG {2210} PARA COMPACTAR ESPACOS DO WKTPART

           IF  WRK-TAM-MSG-NORM        EQUAL ZEROS
            OR WRK-TAM-MSG-NORM        LESS THAN +7
            OR WRK-TAM-MSG-NORM        GREATER +300000
               GO                      TO 3000-99-FIM.

           MOVE +1                     TO WRK-WKT-SCAN-DES.
           MOVE 'S'                    TO WRK-WKT-CONTINUA.

           PERFORM 3010-WKT-BUSCA-TAG
               UNTIL WRK-WKT-CONTINUA  NOT EQUAL 'S'.

      *----------------------------------------------------------------*
       3000-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       3010-WKT-BUSCA-TAG              SECTION.
      *----------------------------------------------------------------*

           MOVE 'N'                    TO WRK-WKT-HIT.

           PERFORM VARYING WRK-WKT-I
                         FROM WRK-WKT-SCAN-DES BY +1
                         UNTIL WRK-WKT-I   GREATER THAN WRK-TAM-MSG-NORM
                            OR WRK-WKT-HIT  EQUAL 'S'

               IF  WRK-MQ-MSGEM-AREA(WRK-WKT-I:6)
                                        EQUAL '{2210}'
                   MOVE 'S'            TO WRK-WKT-HIT
                   COMPUTE WRK-WKT-CSTART
                                        = WRK-WKT-I + 6
               END-IF
           END-PERFORM.

           IF  WRK-WKT-HIT             EQUAL 'N'
               MOVE 'N'                TO WRK-WKT-CONTINUA
               GO                      TO 3010-99-FIM
           END-IF.

           PERFORM 3020-WKT-LOCALIZA-FIM.

      *----------------------------------------------------------------*
       3010-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       3020-WKT-LOCALIZA-FIM           SECTION.
      *----------------------------------------------------------------*

           MOVE 'N'                    TO WRK-WKT-HIT.
           MOVE ZEROS                  TO WRK-NEXT-BRA.

           PERFORM VARYING WRK-WKT-J
                         FROM WRK-WKT-CSTART BY +1
                         UNTIL WRK-WKT-J   GREATER THAN WRK-TAM-MSG-NORM
                            OR WRK-WKT-HIT  EQUAL 'S'

               IF  WRK-MQ-MSGEM-AREA(WRK-WKT-J:1)
                                        EQUAL '{'
                   MOVE 'S'            TO WRK-WKT-HIT
                   MOVE WRK-WKT-J      TO WRK-NEXT-BRA
               END-IF

           END-PERFORM.

           IF  WRK-NEXT-BRA            EQUAL ZEROS
               COMPUTE WRK-NEXT-BRA    = WRK-TAM-MSG-NORM + 1
           END-IF.

           COMPUTE WRK-WKT-IEND        = WRK-NEXT-BRA - 1.
           COMPUTE WRK-WKT-OLD-LEN     = WRK-WKT-IEND
                                       - WRK-WKT-CSTART + 1.

           PERFORM 3030-WKT-COMPACTAR-SEGDO.

           IF  WRK-WKT-DELTA           GREATER THAN ZEROS

               COMPUTE WRK-WKT-TAIL-L  = WRK-TAM-MSG-NORM
                                       - WRK-WKT-IEND

               IF  WRK-WKT-TAIL-L      GREATER THAN ZEROS
                   MOVE WRK-MQ-MSGEM-AREA
                        (WRK-NEXT-BRA:WRK-WKT-TAIL-L)
                                        TO WRK-OVERLAP-MOV
                   COMPUTE WRK-WKT-OUT-IX
                                        = WRK-WKT-CSTART
                                        + WRK-WKT-NEW-LEN
                   MOVE WRK-OVERLAP-MOV(1:WRK-WKT-TAIL-L)
                                        TO WRK-MQ-MSGEM-AREA
                                        (WRK-WKT-OUT-IX:WRK-WKT-TAIL-L)
               END-IF

               COMPUTE WRK-TAM-MSG-NORM = WRK-TAM-MSG-NORM
                                        - WRK-WKT-DELTA
               COMPUTE WRK-WKT-OUT-IX   = WRK-TAM-MSG-NORM + 1
               MOVE SPACES              TO WRK-MQ-MSGEM-AREA
                                          (WRK-WKT-OUT-IX:WRK-WKT-DELTA)

           END-IF.

           COMPUTE WRK-WKT-SCAN-DES     = WRK-WKT-CSTART
                                        + WRK-WKT-NEW-LEN.

      *----------------------------------------------------------------*
       3020-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       3030-WKT-COMPACTAR-SEGDO        SECTION.
      *----------------------------------------------------------------*

           IF  WRK-WKT-OLD-LEN         EQUAL ZEROS
CG2605*     OR WRK-WKT-OLD-LEN         GREATER THAN +300000
            OR WRK-WKT-OLD-LEN         GREATER THAN +10000
               MOVE WRK-WKT-OLD-LEN    TO WRK-WKT-NEW-LEN
               MOVE ZEROS              TO WRK-WKT-DELTA
               GO                      TO 3030-99-FIM.

           MOVE ZEROS                  TO WRK-WKT-NEW-LEN
                                          WRK-WKT-DELTA.
           MOVE SPACES                 TO WRK-WKT-PREV-SP
                                          WRK-WKT-BUF.

           PERFORM VARYING WRK-WKT-OUT-IX
                         FROM WRK-WKT-CSTART BY +1
                        UNTIL WRK-WKT-OUT-IX GREATER THAN WRK-WKT-IEND

               MOVE WRK-MQ-MSGEM-AREA(WRK-WKT-OUT-IX:1)
                                       TO WRK-CH-WKT

               IF  WRK-CH-WKT          EQUAL SPACES
               AND WRK-WKT-PREV-SP     EQUAL SPACES
                   CONTINUE
               ELSE
                   ADD +1              TO WRK-WKT-NEW-LEN
                   MOVE WRK-CH-WKT
                        TO WRK-WKT-BUF(WRK-WKT-NEW-LEN:1)
               END-IF

               MOVE WRK-CH-WKT         TO WRK-WKT-PREV-SP

           END-PERFORM.

           IF  WRK-WKT-NEW-LEN         GREATER THAN ZEROS
               MOVE WRK-WKT-BUF(1:WRK-WKT-NEW-LEN)
                                        TO WRK-MQ-MSGEM-AREA
                                       (WRK-WKT-CSTART:WRK-WKT-NEW-LEN)
           END-IF.

           COMPUTE WRK-WKT-DELTA        = WRK-WKT-OLD-LEN
                                        - WRK-WKT-NEW-LEN.

      *----------------------------------------------------------------*
       3030-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       9000-FORMATAR-ERRO-CICS         SECTION.
      *----------------------------------------------------------------*

           MOVE 'VRS001'               TO ERR-VERSAO.
           MOVE ZEROS                  TO ERR-COD-MSG.
           MOVE SPACES                 TO ERR-TEXTO-MSG.
           MOVE ZEROS                  TO ERR-RETURN-CODE.
           MOVE 'APL'                  TO ERR-TIPO-ACESSO.
           MOVE 'RCOR6007'             TO ERR-PGM.
           MOVE WRK-ERRO-CICS          TO ERR-TEXTO.
           MOVE 'RCOR6007'             TO ERR-MODULO.
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
           MOVE 'RCOR6007'             TO ERR-PGM.
           MOVE WRK-INVOKING-PGM       TO ERR-MODULO.

           PERFORM 9100-GRAVAR-LOG-CDES.

      *----------------------------------------------------------------*
       9200-99-FIM.                    EXIT.
      *----------------------------------------------------------------*
