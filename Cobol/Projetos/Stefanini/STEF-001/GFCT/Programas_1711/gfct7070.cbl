      *================================================================*
       IDENTIFICATION                  DIVISION.
      *================================================================*

       PROGRAM-ID. GFCT7070.
       AUTHOR.     HOMI.
      *================================================================*
      *      S O N D A  I T                                            *
      *----------------------------------------------------------------*
      *    PROGRAMA.........: GFCT7070                                 *
      *    PROGRAMADOR.:   HOMI                                        *
      *    ANALISTA....:   PAGNOCCA - SONDA IT - GR. 50                *
      *    DATA........:   DEZ/2015                                    *
      *----------------------------------------------------------------*
      *    PROJETO..........: BONIFICACAO                              *
      *                                                                *
      *    OBJETIVO.........: ROTEADOR DE CONSULTAS                    *
      *                       SALDO MEDIO APLICACOES FINANCEIRAS       *
      *                                                                *
      *    BOOK'S...........:                                          *
      *        I#GFCTG2 - ENT - OBTER DESCRICAO DE MENSAGEM.           *
      *        I#GFCTG3 - SAI - OBTER DESCRICAO DE MENSAGEM.           *
      *        I#GFCT0M - AREA DE COMUNICACAO - ERROS.                 *
      *                                                                *
      *        GFCTWF19 - ENT - COMBO/CONSULTA                         *
      *        GFCTWF20 - SAI - COMBO/CONSULTA                         *
      *                                                                *
      *    MODULOS CHAMADOS.:                                          *
      *        GFCT0350 - COMBO/CONSULTA                               *
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

      *================================================================*
       DATA                            DIVISION.
      *================================================================*

      *----------------------------------------------------------------*
       WORKING-STORAGE                 SECTION.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       01  FILLER                      PIC X(32)           VALUE
           '*  INICIO DA WORKING GFCT7070  *'.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       01  FILLER                      PIC X(32)           VALUE
           '*     AREA DE FUNCOES SBAT     *'.
      *----------------------------------------------------------------*

       01  WRK-AREAS-SBAT.
           05  WRK-GU                  PIC X(04)           VALUE 'GU'.
           05  WRK-MSGF                PIC X(04)           VALUE 'MSGF'.
           05  WRK-MSGI                PIC X(04)           VALUE 'MSGI'.
           05  WRK-RSTA                PIC X(04)           VALUE 'RSTA'.
           05  WRK-ERRO                PIC X(04)           VALUE 'ERRO'.

      *----------------------------------------------------------------*
       01  FILLER                      PIC X(32)           VALUE
           '*     INDEXADORES     *'.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       01  FILLER                      PIC X(32)           VALUE
           '*     VARIAVEIS AUXILIARES     *'.
      *----------------------------------------------------------------*

       01  WRK-AUXILIARES.
           05  WRK-FUNCAO              PIC X(04)           VALUE SPACES.
           05  WRK-MODULO              PIC X(08)           VALUE SPACES.
           05  WRK-X-1010.
             10  WRK-X-300             PIC X(300)          VALUE SPACES.
             10  FILLER                PIC X(710)          VALUE SPACES.

      *----------------------------------------------------------------*
       01  FILLER                      PIC X(32)           VALUE
           '* CODIGOS/MENSAGENS DE RETORNO *'.
      *----------------------------------------------------------------*

       01  WRK-MENSAGENS.
           05 WRK-MSG001               PIC X(75)           VALUE
              'ERRO NO SBAT3000 PARA OBTER A MENSAGEM DE ENTRADA'.
           05 WRK-MSG002               PIC X(75)           VALUE
              'ERRO NO SBAT3000 PARA ENVIAR MENSAGEM - MSGI'.
           05 WRK-MSG003               PIC X(75)           VALUE
              'ERRO NO SBAT3000 PARA ENVIAR MENSAGEM - MSGF'.
           05 WRK-MSG004               PIC X(75)           VALUE
              'ERRO NO SBAT3000 PARA ENVIAR MENSAGEM - RSTA'.
           05 WRK-MSG005.
             10  FILLER                PIC X(26)           VALUE
                 'ERRO NA CHAMADA DO MODULO'.
             10  WRK-MODULO-MSG        PIC X(08)           VALUE SPACES.

      *---------------------------------------------------------------*
       01  FILLER                      PIC X(50)           VALUE
           '* AREA PARA INCLUDES DO PROGRAMA *'.
      *---------------------------------------------------------------*

       COPY 'I#ISDDPC'.
       COPY 'I#GFCT0K'.
       COPY 'I#GFCT0L'.
       COPY 'I#GFCT0M'.
       COPY 'I#GFCT3O'.

       COPY 'GFCTWF19'.
       COPY 'GFCTWF20'.

      *----------------------------------------------------------------*
       01  FILLER                      PIC X(32)           VALUE
           '*    FIM DA WORKING GFCT7070   *'.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       LINKAGE                         SECTION.
      *----------------------------------------------------------------*

       01  LNK-IO-PCB.
           05  LNK-IO-TERM             PIC  X(08)          VALUE SPACES.
           05  FILLER                  PIC  X(02)          VALUE SPACES.
           05  LNK-IO-STA              PIC  X(02)          VALUE SPACES.
           05  FILLER                  PIC  X(12)          VALUE SPACES.
           05  LNK-IO-MODNAME          PIC  X(08)          VALUE SPACES.

       01  LNK-ALT-PCB.
           05  LNK-ALT-PCBNAME         PIC  X(08)          VALUE SPACES.
           05  FILLER                  PIC  X(02)          VALUE SPACES.
           05  LNK-ALT-STATUS          PIC  X(02)          VALUE SPACES.
           05  LNK-ALT-DATE            PIC S9(07) COMP-3   VALUE ZEROS.
           05  LNK-ALT-TIME            PIC S9(07) COMP-3   VALUE ZEROS.
           05  LNK-ALT-SEQNO           PIC S9(03) COMP     VALUE ZEROS.
           05  FILLER                  PIC  X(02)          VALUE SPACES.
           05  LNK-ALT-MODNAME         PIC  X(08)          VALUE SPACES.

      *================================================================*
       PROCEDURE                       DIVISION.
      *================================================================*

           ENTRY  'DLITCBL'            USING LNK-IO-PCB
                                             LNK-ALT-PCB.

      *----------------------------------------------------------------*
      *    ROTINA INICIAL                                              *
      *----------------------------------------------------------------*
       0000-INICIAR                    SECTION.
      *----------------------------------------------------------------*

           MOVE SPACES                 TO GFCT0M-AREA-ERROS.

           INITIALIZE                  GFCT0K-AREA
                                       GFCT0L-AREA
                                       GFCT0M-AREA-ERROS
                                       GFCT3O-AREA-RESTART
                                       WF19-ENTRADA
                                       WF20-SAIDA.

           PERFORM 1000-LER-MENSAGEM.

           PERFORM 2000-ROTINA-PRINCIPAL.

           GO                          TO 0000-INICIAR.

      *----------------------------------------------------------------*
       0000-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *    ROTINA PARA LER MENSAGEM                                    *
      *----------------------------------------------------------------*
       1000-LER-MENSAGEM               SECTION.
      *----------------------------------------------------------------*

           MOVE WRK-GU                 TO WRK-FUNCAO.

           CALL 'SBAT3000'             USING WRK-FUNCAO
                                             LNK-IO-PCB
                                             LNK-ALT-PCB
                                             MCA-ISDPA
                                             GFCT0K-ENTRADA.

           IF  MPA-RCODE-API           EQUAL 'QC'
               GOBACK
           END-IF.

           IF  MPA-RCODE-API           NOT EQUAL SPACES
               MOVE WRK-MSG001         TO GFCT0M-TEXTO
               PERFORM 9998-FORMATAR-ERRO
               PERFORM 9999-TRATAR-ERRO
           END-IF.

      *----------------------------------------------------------------*
       1000-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *    ROTINA PRINCIPAL                                            *
      *----------------------------------------------------------------*
       2000-ROTINA-PRINCIPAL           SECTION.
      *----------------------------------------------------------------*

           EVALUATE GFCT0K-FUNCAO

               WHEN 'GFCT0350'
                   MOVE GFCT0K-ENTRADA TO WRK-X-1010
                   MOVE WRK-X-300      TO WF19-ENTRADA
                   PERFORM 2100-PROCESSAR

           END-EVALUATE.

      *----------------------------------------------------------------*
       2000-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *    ROTINA PARA PROCESSAR INCLUSAO / EXCLUSAO                   *
      *----------------------------------------------------------------*
       2100-PROCESSAR                  SECTION.
      *----------------------------------------------------------------*

           MOVE SPACES                 TO WF20-SAIDA

           INITIALIZE WF20-SAIDA

           PERFORM 2110-CHAMAR-GFCT0350

           MOVE WF20-SAIDA         TO GFCT0L-ENTRADA

           PERFORM 2120-TRATAR-RETORNO

           PERFORM 2130-CHAMAR-SBAT3000.

      *----------------------------------------------------------------*
       2100-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *    ROTINA PARA ACESSAR O MODULO GFCT0350                       *
      *----------------------------------------------------------------*
       2110-CHAMAR-GFCT0350            SECTION.
      *----------------------------------------------------------------*

           MOVE '00'                   TO GFCT0L-MPARCODE-APLIC.
           MOVE WF19-FUNCAO            TO WRK-MODULO.

           CALL WRK-MODULO             USING WF19-ENTRADA
                                             WF20-SAIDA
                                             GFCT0M-AREA-ERROS.

           IF  RETURN-CODE             NOT EQUAL ZEROS
               MOVE ZEROS              TO RETURN-CODE
               MOVE WRK-MODULO         TO WRK-MODULO-MSG
               MOVE WRK-MSG005         TO GFCT0M-TEXTO(1:34)
               PERFORM 9998-FORMATAR-ERRO
               PERFORM 9999-TRATAR-ERRO
           END-IF.

           IF  WF20-ERRO            EQUAL 9
               PERFORM 9999-TRATAR-ERRO
           END-IF.

      *----------------------------------------------------------------*
       2110-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *    ROTINA PARA TRATAR RETORNO DE ACESSO AO MODULO              *
      *----------------------------------------------------------------*
       2120-TRATAR-RETORNO             SECTION.
      *----------------------------------------------------------------*

           MOVE ZEROS                  TO GFCT0L-NRO-MSGI.
           MOVE '00'                   TO GFCT0L-MPARCODE-APLIC.
           MOVE WRK-MSGF               TO GFCT0L-TPOMSG-WEB.
           MOVE GFCT0L-TPOMSG-WEB      TO WRK-FUNCAO.

      *----------------------------------------------------------------*
       2120-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *    ROTINA PARA ENVIAR BLOCO                                    *
      *----------------------------------------------------------------*
       2130-CHAMAR-SBAT3000            SECTION.
      *----------------------------------------------------------------*

           IF  WRK-FUNCAO              EQUAL WRK-RSTA
               CALL 'SBAT3000'         USING WRK-FUNCAO
                                             LNK-IO-PCB
                                             LNK-ALT-PCB
                                             MCA-ISDPA
                                             GFCT0L-ENTRADA
                                             GFCT3O-AREA-RESTART
           ELSE
               CALL 'SBAT3000'         USING WRK-FUNCAO
                                             LNK-IO-PCB
                                             LNK-ALT-PCB
                                             MCA-ISDPA
                                             GFCT0L-ENTRADA
           END-IF.

           IF  MPA-RCODE-API           NOT EQUAL SPACES
               IF  WRK-FUNCAO          EQUAL WRK-MSGI
                   MOVE WRK-MSG002     TO GFCT0M-TEXTO
               ELSE
                   IF  WRK-FUNCAO      EQUAL WRK-MSGF
                       MOVE WRK-MSG003 TO GFCT0M-TEXTO
                   ELSE
                       MOVE WRK-MSG004 TO GFCT0M-TEXTO
                   END-IF
               END-IF
               PERFORM 9998-FORMATAR-ERRO
               PERFORM 9999-TRATAR-ERRO
           END-IF.

      *----------------------------------------------------------------*
       2130-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *    FORMATAR ERRO                                               *
      *----------------------------------------------------------------*
       9998-FORMATAR-ERRO              SECTION.
      *----------------------------------------------------------------*

           MOVE 'APL'                  TO GFCT0M-TIPO-ACESSO.
           MOVE 'GFCT7070'             TO GFCT0M-TRANSACAO.
           MOVE '02'                   TO GFCT0L-MPARCODE-APLIC.

      *----------------------------------------------------------------*
       9998-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *    TRATAMENTO DE ERRO                                          *
      *----------------------------------------------------------------*
       9999-TRATAR-ERRO                SECTION.
      *----------------------------------------------------------------*

           MOVE 'GFCT7070'             TO GFCT0M-PROGRAMA.
           MOVE GFCT0L-MPARCODE-APLIC  TO MPA-RCODE-APLICACAO.
           MOVE WRK-ERRO               TO WRK-FUNCAO.

           CALL 'SBAT3000'         USING WRK-FUNCAO
                                         LNK-IO-PCB
                                         LNK-ALT-PCB
                                         MCA-ISDPA
                                         GFCT0M-ERRO-AREAS.

           GOBACK.

      *----------------------------------------------------------------*
       9999-99-FIM.                    EXIT.
      *----------------------------------------------------------------*
