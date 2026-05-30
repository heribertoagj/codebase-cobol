      *================================================================*
       IDENTIFICATION                  DIVISION.
      *================================================================*

       PROGRAM-ID. GFCT7940.
       AUTHOR. MAIKON JEFERSON FERREIRA.
      *================================================================*
      *                    C P M   S I S T E M A S                     *
      *----------------------------------------------------------------*
      *                                                                *
      *    PROGRAMA     : GFCT7940                                     *
      *    PROGRAMADOR  : MAIKON JEFERSON FERREIRA      - CPM/FPOLIS   *
      *    ANALISTA CPM : ALEXANDRE PEREIRA             - CPM/FPOLIS   *
      *    ANALISTA     : VANI NUNES - PROCWORK         - GRUPO 50     *
      *    DATA         : 22/01/2007                                   *
      *                                                                *
      *    OBJETIVO     :                                              *
      *      EFETUAR COMUNICACAO ENTRE WEB E MF VIA SBAT3000 PARA AS   *
      *      CHAMADAS NOS MODULOS REFERENTES A FUNCIONALIDADE DE LI-   *
      *      BERACAO DE TARIFAS SUSPENSAS POR AGRUPAMENTO.             *
      *                                                                *
      *    MODULOS CHAMADOS:                                           *
      *      SBAT3000 - MODULO DE COMUNICACAO ENTRE PLATAFORMAS.       *
      *      POOL0081 - ALOCAR DINAMICAMENTE UM MODULO.                *
      *      GFCT0938 - TRATAR LIBERACAO.                              *
      *      GFCT0940 - LISTA TARIFAS SUSPENSAS.                       *
      *      GFCT0959 - CANCEL. SOLIC. DE LIBER. DE SUSP. COBR. TARIFA *
      *      GFCT0960 - CONS. SOLIC. DE LIBER. DE SUSP. COBR. TARIFA   *
      *      GFCT5574 - TRATAR AGRUPAMENTO.                            *
      *      GFCT5575 - TRATAR TARIFA AGRUPAMENTO.                     *
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
           '*  INICIO DA WORKING GFCT7940  *'.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       01  FILLER                      PIC X(32)           VALUE
           '*        INDEXADORES           *'.
      *----------------------------------------------------------------*


       01  IND-INDEDADORES.
           05 IND-1                    PIC 9(03) COMP-3    VALUE ZEROS.

      *----------------------------------------------------------------*
       01  FILLER                      PIC X(32)           VALUE
           '*     VARIAVEIS AUXILIARES     *'.
      *----------------------------------------------------------------*

       01  WRK-AUXILIARES.
           05  WRK-MODULO              PIC X(08)           VALUE SPACES.

      *----------------------------------------------------------------*
       01  FILLER                      PIC X(32)           VALUE
           '*     AREA DE FUNCOES SBAT3000 *'.
      *----------------------------------------------------------------*

       01  WRK-FUNCAO                  PIC X(04)           VALUE SPACES.
       01  WRK-GU                      PIC X(04)           VALUE 'GU'.
       01  WRK-MSGF                    PIC X(04)           VALUE 'MSGF'.
       01  WRK-ERRO                    PIC X(04)           VALUE 'ERRO'.
       01  WRK-MSGI                    PIC X(04)           VALUE 'MSGI'.

       01  WRK-RSTA                    PIC X(04)           VALUE 'RSTA'.

      *----------------------------------------------------------------*
       01  FILLER                      PIC X(32)           VALUE
           '*     AREA DE MENSAGENS        *'.
      *----------------------------------------------------------------*

       01  WRK-MENSAGENS.
           05 WRK-MSG01                PIC X(75)           VALUE
              'ERRO NO SBAT3000 PARA OBETER A MENSAGEM DE ENTRADA'.
           05 WRK-MSG02                PIC X(75)           VALUE
              'ERRO NO SBAT3000 PARA ENVIAR MENSAGEM - MSGI'.
           05 WRK-MSG03                PIC X(75)           VALUE
              'ERRO NO SBAT3000 PARA ENVIAR MENSAGEM - MSGF'.
           05 WRK-MSG04                PIC X(75)           VALUE
              'ERRO NO SBAT3000 PARA ENVIAR MENSAGEM - RSTA'.
           05 WRK-MSG05.
             10 FILLER                 PIC X(26)           VALUE
                'ERRO NA CHAMADA DO MODULO'.
             10 WRK-MODULO-MSG05       PIC X(08)           VALUE SPACES.
             10 FILLER                 PIC X(41)           VALUE SPACES.


      *----------------------------------------------------------------*
       01  FILLER                      PIC X(32)           VALUE
           '*      AREA PARA I#GFCTFZ     *'.
      *----------------------------------------------------------------*

       COPY I#GFCTFZ.

      *----------------------------------------------------------------*
       01  FILLER                      PIC X(32)           VALUE
           '*      AREA PARA I#GFCTG1     *'.
      *----------------------------------------------------------------*

       COPY I#GFCTG1.

      *----------------------------------------------------------------*
       01  FILLER                      PIC X(32)           VALUE
           '*      AREA PARA I#GFCTY0     *'.
      *----------------------------------------------------------------*

       COPY I#GFCTY0.


      *----------------------------------------------------------------*
       01  FILLER                      PIC X(32)           VALUE
           '*      AREA PARA I#GFCTY1     *'.
      *----------------------------------------------------------------*

       COPY I#GFCTY1.

      *----------------------------------------------------------------*
       01  FILLER                      PIC X(32)           VALUE
           '*      AREA PARA I#GFCTYA     *'.
      *----------------------------------------------------------------*

       COPY I#GFCTYA.

      *----------------------------------------------------------------*
       01  FILLER                      PIC X(32)           VALUE
           '*      AREA PARA I#GFCTYB     *'.
      *----------------------------------------------------------------*

       COPY I#GFCTYB.


      *----------------------------------------------------------------*
       01  FILLER                      PIC X(32)           VALUE
           '*      AREA PARA I#GFCTY8     *'.
      *----------------------------------------------------------------*

       COPY I#GFCTY8.

      *----------------------------------------------------------------*
       01  FILLER                      PIC X(32)           VALUE
           '*      AREA PARA I#GFCTY9     *'.
      *----------------------------------------------------------------*

       COPY I#GFCTY9.

      *----------------------------------------------------------------*
       01  FILLER                      PIC X(32)           VALUE
           '*      AREA PARA I#GFCTYC     *'.
      *----------------------------------------------------------------*

       COPY I#GFCTYC.


      *----------------------------------------------------------------*
       01  FILLER                      PIC X(32)           VALUE
           '*      AREA PARA I#GFCTYD     *'.
      *----------------------------------------------------------------*

       COPY I#GFCTYD.

      *----------------------------------------------------------------*
       01  FILLER                      PIC X(32)           VALUE
           '*      AREA PARA I#GFCTWE     *'.
      *----------------------------------------------------------------*

       COPY I#GFCTWE.

      *----------------------------------------------------------------*
       01  FILLER                      PIC X(32)           VALUE
           '*      AREA PARA I#GFCTWF     *'.
      *----------------------------------------------------------------*

       COPY I#GFCTWF.


      *----------------------------------------------------------------*
       01  FILLER                      PIC X(32)           VALUE
           '*      AREA PARA I#GFCTWG     *'.
      *----------------------------------------------------------------*

       COPY I#GFCTWG.

      *----------------------------------------------------------------*
       01  FILLER                      PIC X(32)           VALUE
           '*      AREA PARA I#GFCTWH     *'.
      *----------------------------------------------------------------*

       COPY I#GFCTWH.

      *----------------------------------------------------------------*
       01  FILLER                      PIC X(32)           VALUE
           '*      AREA PARA I#GFCT0M     *'.
      *----------------------------------------------------------------*

       COPY I#GFCT0M.


      *----------------------------------------------------------------*
       01  FILLER                      PIC X(32)           VALUE
           '*      AREA PARA I#GFCT3O     *'.
      *----------------------------------------------------------------*

       COPY I#GFCT3O.

      *----------------------------------------------------------------*
       01  FILLER                      PIC X(32)           VALUE
           '*      AREA PARA I#GFCT0K     *'.
      *----------------------------------------------------------------*

       COPY I#GFCT0K.

      *----------------------------------------------------------------*
       01  FILLER                      PIC X(32)           VALUE
           '*      AREA PARA I#GFCT0L     *'.
      *----------------------------------------------------------------*

       COPY I#GFCT0L.


      *----------------------------------------------------------------*
       01  FILLER                      PIC X(34)           VALUE
           '*  MCA - AREA DE COMUN. SBAT3000 *'.
      *----------------------------------------------------------------*

       COPY I#ISDDPC.

      *----------------------------------------------------------------*
       01  FILLER                      PIC X(32)           VALUE
           '*    FIM DA WORKING GFCT7940   *'.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       LINKAGE                         SECTION.
      *----------------------------------------------------------------*

       01  LNK-IO-PCB.
           05  LNK-IO-TERM             PIC  X(08).
           05  FILLER                  PIC  X(02).
           05  LNK-IO-STA              PIC  X(02).

           05  FILLER                  PIC  X(12).
           05  LNK-IO-MODNAME          PIC  X(08).

       01  LNK-ALT-PCB.
           05  LNK-ALT-PCBNAME         PIC  X(08).
           05  FILLER                  PIC  X(02).
           05  LNK-ALT-STATUS          PIC  X(02).
           05  LNK-ALT-DATE            PIC S9(07) COMP-3.
           05  LNK-ALT-TIME            PIC S9(07) COMP-3.
           05  LNK-ALT-SEQNO           PIC S9(03) COMP.
           05  FILLER                  PIC  X(02).
           05  LNK-ALT-MODNAME         PIC  X(08).

      *================================================================*
       PROCEDURE                       DIVISION.
      *================================================================*

           ENTRY  'DLITCBL'            USING LNK-IO-PCB
                                             LNK-ALT-PCB.

      *----------------------------------------------------------------*

      *    ROTINA PRINCIPAL                                            *
      *----------------------------------------------------------------*
       0000-PROCEDIMENTOS-INICIAIS     SECTION.
      *----------------------------------------------------------------*

           MOVE SPACES                 TO GFCT0K-AREA
                                          GFCT0L-AREA
                                          GFCT0M-AREA-ERROS
                                          GFCT3O-AREA-RESTART.

           INITIALIZE GFCT0K-AREA
                      GFCT0L-AREA
                      GFCT0M-AREA-ERROS
                      GFCT3O-AREA-RESTART.

           MOVE WRK-GU                 TO WRK-FUNCAO.
           MOVE '00'                   TO GFCT0L-MPARCODE-APLIC.

           CALL 'SBAT3000'             USING WRK-FUNCAO
                                             LNK-IO-PCB
                                             LNK-ALT-PCB

                                             MCA-ISDPA
                                             GFCT0K-ENTRADA.

           IF  MPA-RCODE-API           EQUAL 'QC'
               GOBACK
           END-IF.

           IF  MPA-RCODE-API           NOT EQUAL SPACES
               MOVE WRK-MSG01          TO GFCT0M-TEXTO
               MOVE '02'               TO GFCT0L-MPARCODE-APLIC
               MOVE 'APL'              TO GFCT0M-TIPO-ACESSO
               MOVE 'GFCT7940'         TO GFCT0M-PROGRAMA
               MOVE  WRK-MSG01         TO GFCT0M-TEXTO
               PERFORM 9999-PROCESSAR-ROTINA-ERRO
           END-IF.

           PERFORM 0100-PROCESSAR.

           GO                          TO 0000-PROCEDIMENTOS-INICIAIS.

      *----------------------------------------------------------------*

       0000-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *    ROTINA DE PROCESSAMENTO PRINCIPAL DO PROGRAMA               *
      *----------------------------------------------------------------*
       0100-PROCESSAR                  SECTION.
      *----------------------------------------------------------------*

           PERFORM 0200-VERIFICAR-DISPONIVEL.

           INITIALIZE GFCT0L-RET-WEB.

           EVALUATE GFCT0K-FUNCAO
               WHEN 'GFCT0940'
                   MOVE GFCT0K-ENTRADA TO GFCTY0-ENTRADA
                   MOVE 'N'            TO GFCTY1-FIM
                   PERFORM 0300-TRATAR-LISTA-TARIFAS-SUSP
                           VARYING IND-1   FROM 1 BY 1
                           UNTIL   IND-1   GREATER 3   OR
                           GFCTY1-FIM      EQUAL 'S'


               WHEN 'GFCT0938'
                   MOVE GFCT0K-ENTRADA TO GFCTYA-ENTRADA
                   MOVE 'N'            TO GFCTYB-FIM
                   PERFORM 0700-TRATAR-LIBERACAO

               WHEN 'GFCT0959'
                   MOVE GFCT0K-ENTRADA TO GFCTWE-ENTRADA
                   MOVE 'N'            TO GFCTWF-FIM
                   PERFORM 1500-TRATAR-CANCELAMENTO

               WHEN 'GFCT0960'
                   MOVE GFCT0K-ENTRADA TO GFCTWG-ENTRADA
                   MOVE 'N'            TO GFCTWH-FIM
                   PERFORM 1600-TRATAR-CONS-TARIFAS-SUSP
                           VARYING IND-1   FROM 1 BY 1
                           UNTIL   IND-1   GREATER 3   OR
                           GFCTWH-FIM      EQUAL 'S'

               WHEN 'GFCT5574'
                   MOVE GFCT0K-ENTRADA TO GFCTY8-ENTRADA

                   MOVE 'N'            TO GFCTY9-FIM
                   PERFORM 0900-TRATAR-COMB-AGRUPTO
                           VARYING IND-1   FROM 1 BY 1
                           UNTIL   IND-1   GREATER 3   OR
                           GFCTY9-FIM      EQUAL 'S'

               WHEN 'GFCT5575'
                   MOVE GFCT0K-ENTRADA TO GFCTYC-ENTRADA
                   MOVE 'N'            TO GFCTYD-FIM
                   PERFORM 1200-TRATAR-COMB-TARIF-AGR
                           VARYING IND-1   FROM 1 BY 1
                           UNTIL   IND-1   GREATER 3   OR
                           GFCTYD-FIM      EQUAL 'S'
           END-EVALUATE.

      *----------------------------------------------------------------*
       0100-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *    ROTINA PARA VERIFICAR DISPONIBILIDADE DO PROGRAMA           *

      *----------------------------------------------------------------*
       0200-VERIFICAR-DISPONIVEL       SECTION.
      *----------------------------------------------------------------*

           MOVE SPACES                 TO GFCTFZ-ENTRADA
                                          GFCTG1-SAIDA
                                          GFCT0M-AREA-ERROS.

           INITIALIZE GFCTFZ-ENTRADA
                      GFCTG1-SAIDA
                      GFCT0M-AREA-ERROS.

           MOVE +101                   TO GFCTFZ-LL.
           MOVE 'GFCT'                 TO GFCTFZ-SISTEMA.
           MOVE  GFCT0K-TRANSACAO      TO GFCTFZ-TRANSACAO.
           MOVE  GFCT0K-FUNCAO         TO GFCTFZ-FUNCAO.
           MOVE '0000000'              TO GFCTFZ-FUNC-BDSCO.
           MOVE 'N'                    TO GFCTFZ-FIM.
           MOVE 'GFCT5522'             TO WRK-MODULO.

           CALL 'POOL0081'             USING WRK-MODULO

                                             GFCTFZ-ENTRADA
                                             GFCTG1-SAIDA
                                             GFCT0M-AREA-ERROS.

           IF  RETURN-CODE             NOT EQUAL ZEROS
               PERFORM 9997-TRATAR-ERRO-MODULO
           END-IF.

           IF  GFCTG1-ERRO             EQUAL 9
               PERFORM 9998-TRATAR-ERRO-APL-DB2
           END-IF.

           IF  GFCTG1-CSIT-ONLINE      EQUAL 1
               MOVE 'APL'
                                       TO GFCT0M-TIPO-ACESSO
               MOVE 'SISTEMA INDISPONIVEL'
                                       TO GFCT0M-TEXTO
               MOVE WRK-MODULO         TO GFCT0M-PROGRAMA
               PERFORM 9999-PROCESSAR-ROTINA-ERRO
           END-IF.


      *----------------------------------------------------------------*
       0200-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *    ROTINA PARA TRATAR LISTA DE TAREFAS SUSPENSAS               *
      *----------------------------------------------------------------*
       0300-TRATAR-LISTA-TARIFAS-SUSP  SECTION.
      *----------------------------------------------------------------*

           IF  GFCTY0-FIM              EQUAL 'N'
               MOVE SPACES             TO GFCTY1-SAIDA
                                          GFCT0M-AREA-ERROS
               INITIALIZE GFCT0L-RET-WEB
                          GFCTY1-SAIDA
                          GFCT0M-AREA-ERROS
           END-IF.

           PERFORM 0400-CHAMA-GFCT0940.

      *----------------------------------------------------------------*

       0300-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *    ROTINA PARA CHAMAR O MODULO GFCT0940                        *
      *----------------------------------------------------------------*
       0400-CHAMA-GFCT0940             SECTION.
      *----------------------------------------------------------------*

           MOVE GFCTY0-FUNCAO          TO WRK-MODULO.

           CALL 'POOL0081'             USING WRK-MODULO
                                             GFCTY0-ENTRADA
                                             GFCTY1-SAIDA
                                             GFCT0M-AREA-ERROS.

           IF  RETURN-CODE             NOT EQUAL ZEROS
               PERFORM 9997-TRATAR-ERRO-MODULO
           END-IF.

           IF  GFCTY1-ERRO             EQUAL 9

               PERFORM 9998-TRATAR-ERRO-APL-DB2
           END-IF.

           MOVE GFCTY1-SAIDA           TO GFCT0L-ENTRADA.

           IF (GFCTY1-FIM              EQUAL 'S') AND
              (IND-1                   LESS 3 )
               MOVE WRK-MSGF           TO GFCT0L-TPOMSG-WEB
                                          WRK-FUNCAO
               MOVE ZEROS              TO GFCT0L-NRO-MSGI
           ELSE
               IF (IND-1               LESS 3)
                   MOVE WRK-MSGI       TO GFCT0L-TPOMSG-WEB
                                          WRK-FUNCAO
                   MOVE GFCTY1-FILTRO  TO GFCTY0-FILTRO
                   MOVE GFCTY1-PONTEIRO
                                       TO GFCTY0-PONTEIRO
                   MOVE GFCTY1-QTDE-OCOR
                                       TO GFCTY0-QTDE-OCOR
               ELSE
                   MOVE WRK-RSTA       TO GFCT0L-TPOMSG-WEB

                                          WRK-FUNCAO
                   MOVE ZEROS          TO GFCT0L-NRO-MSGI
                   MOVE 'S'            TO GFCTY1-FIM
               END-IF
           END-IF.

           IF  GFCT0L-TPOMSG-WEB       EQUAL  WRK-RSTA OR WRK-MSGF
               MOVE ZEROS              TO GFCT0L-NRO-MSGI
           ELSE
               ADD 1                   TO GFCT0L-NRO-MSGI
           END-IF.

           IF  WRK-FUNCAO              EQUAL WRK-RSTA
               PERFORM 0500-RETORNO-GFCT0940
           END-IF.

           PERFORM 0600-CHAMAR-SBAT3000.

      *----------------------------------------------------------------*
       0400-99-FIM.                    EXIT.
      *----------------------------------------------------------------*


      *----------------------------------------------------------------*
      *    ROTINA PARA RETORNO DO MODULO GFCT0940                      *
      *----------------------------------------------------------------*
       0500-RETORNO-GFCT0940           SECTION.
      *----------------------------------------------------------------*

           MOVE +150                   TO GFCT3O-LL.
           MOVE GFCTY1-FUNCAO          TO GFCT3O-FUNCAO.
           MOVE GFCTY1-FILTRO          TO GFCTY0-FILTRO.
           MOVE GFCTY1-PONTEIRO        TO GFCTY0-PONTEIRO.
           MOVE GFCTY1-QTDE-OCOR       TO GFCTY0-QTDE-OCOR.
           MOVE ZEROS                  TO GFCTY0-QTDE-TOT-REG.
           MOVE GFCTY1-FUNC-BDSCO      TO GFCTY0-FUNC-BDSCO.
           MOVE GFCTY1-FIM             TO GFCTY0-FIM.
           MOVE GFCTY0-DADOS-RST       TO GFCT3O-DADOS-RST.

      *----------------------------------------------------------------*
       0500-99-FIM.                    EXIT.
      *----------------------------------------------------------------*


      *----------------------------------------------------------------*
      *    ROTINA PARA CHAMAR O MODULO SBAT3000                        *
      *----------------------------------------------------------------*
       0600-CHAMAR-SBAT3000            SECTION.
      *----------------------------------------------------------------*

           IF  WRK-FUNCAO              EQUAL 'RSTA'
               CALL 'SBAT3000'         USING  WRK-FUNCAO
                                              LNK-IO-PCB
                                              LNK-ALT-PCB
                                              MCA-ISDPA
                                              GFCT0L-ENTRADA
                                              GFCT3O-AREA-RESTART
           ELSE
               CALL 'SBAT3000'         USING  WRK-FUNCAO
                                              LNK-IO-PCB
                                              LNK-ALT-PCB
                                              MCA-ISDPA
                                              GFCT0L-ENTRADA
           END-IF.


           IF  MPA-RCODE-API           NOT EQUAL SPACES
               IF WRK-FUNCAO           EQUAL 'MSGI'
                  MOVE WRK-MSG02       TO GFCT0M-TEXTO
               ELSE
                   IF  WRK-FUNCAO      EQUAL 'MSGF'
                       MOVE WRK-MSG03  TO GFCT0M-TEXTO
                   ELSE
                       MOVE WRK-MSG04  TO GFCT0M-TEXTO
                   END-IF
               END-IF
               MOVE '02'               TO GFCT0L-MPARCODE-APLIC
               MOVE 'GFCT7940'         TO GFCT0M-PROGRAMA
               MOVE 'APL'              TO GFCT0M-TIPO-ACESSO
               PERFORM 9999-PROCESSAR-ROTINA-ERRO
           END-IF.

      *----------------------------------------------------------------*
       0600-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*

      *    ROTINA PARA TRATAR LIBERACAO                                *
      *----------------------------------------------------------------*
       0700-TRATAR-LIBERACAO           SECTION.
      *----------------------------------------------------------------*

           MOVE SPACES                 TO GFCTYB-SAIDA
                                          GFCT0M-AREA-ERROS.

           INITIALIZE GFCT0L-RET-WEB
                      GFCTYB-SAIDA
                      GFCT0M-AREA-ERROS.

           PERFORM 0800-CHAMAR-GFCT0938.

      *----------------------------------------------------------------*
       0700-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *    ROTINA PARA CHAMAR MODULO GFCT0938                          *
      *----------------------------------------------------------------*

       0800-CHAMAR-GFCT0938            SECTION.
      *----------------------------------------------------------------*

           MOVE GFCTYA-FUNCAO          TO WRK-MODULO.

           CALL 'POOL0081'             USING WRK-MODULO
                                             GFCTYA-ENTRADA
                                             GFCTYB-SAIDA
                                             GFCT0M-AREA-ERROS.

           IF  RETURN-CODE             NOT EQUAL ZEROS
               PERFORM 9997-TRATAR-ERRO-MODULO
           END-IF.

           IF  GFCTYB-ERRO             EQUAL 9
               PERFORM 9998-TRATAR-ERRO-APL-DB2
           END-IF.

           MOVE GFCTYB-SAIDA           TO GFCT0L-ENTRADA.

           IF  GFCTYB-FIM              EQUAL 'S'

               MOVE WRK-MSGF           TO GFCT0L-TPOMSG-WEB
                                          WRK-FUNCAO
               MOVE ZEROS              TO GFCT0L-NRO-MSGI
           ELSE
               MOVE WRK-MSGI           TO GFCT0L-TPOMSG-WEB
                                          WRK-FUNCAO
               ADD  1                  TO GFCT0L-NRO-MSGI
           END-IF.

           PERFORM 0600-CHAMAR-SBAT3000.

      *----------------------------------------------------------------*
       0800-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *    ROTINA PARA TRATAR AGRUPAMENTO                              *
      *----------------------------------------------------------------*
       0900-TRATAR-COMB-AGRUPTO        SECTION.
      *----------------------------------------------------------------*


           IF  GFCTY8-FIM              EQUAL  'N'
               MOVE SPACES             TO GFCTY9-SAIDA
                                          GFCT0M-AREA-ERROS
               INITIALIZE GFCT0L-RET-WEB
                          GFCTY9-SAIDA
                          GFCT0M-AREA-ERROS
           END-IF

           MOVE '00'                   TO GFCT0L-MPARCODE-APLIC.

           PERFORM 1000-CHAMAR-GFCT5574.

      *----------------------------------------------------------------*
       0900-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *    ROTINA PARA CHAMAR MODULO GFCT5574                          *
      *----------------------------------------------------------------*
       1000-CHAMAR-GFCT5574            SECTION.
      *----------------------------------------------------------------*


           MOVE GFCTY8-FUNCAO          TO WRK-MODULO.

           CALL 'POOL0081'             USING WRK-MODULO
                                             GFCTY8-ENTRADA
                                             GFCTY9-SAIDA
                                             GFCT0M-AREA-ERROS.

           IF (RETURN-CODE             NOT EQUAL ZEROS)
               PERFORM 9997-TRATAR-ERRO-MODULO
           END-IF.

           IF  GFCTY9-ERRO             EQUAL 9
               PERFORM 9998-TRATAR-ERRO-APL-DB2
           END-IF.

           MOVE GFCTY9-SAIDA           TO GFCT0L-ENTRADA.
           MOVE '00'                   TO GFCT0L-MPARCODE-APLIC.

           IF (GFCTY9-FIM              EQUAL 'S') AND
              (IND-1                   LESS   3 )

               MOVE WRK-MSGF           TO GFCT0L-TPOMSG-WEB
                                          WRK-FUNCAO
               MOVE ZEROS              TO GFCT0L-NRO-MSGI
           ELSE
               IF (IND-1               LESS 3)
                   MOVE WRK-MSGI       TO GFCT0L-TPOMSG-WEB
                                          WRK-FUNCAO
                   MOVE GFCTY9-FILTRO  TO GFCTY8-FILTRO
                   MOVE GFCTY9-PONTEIRO
                                       TO GFCTY8-PONTEIRO
                   ADD  1              TO GFCT0L-NRO-MSGI
               ELSE
                   MOVE WRK-RSTA       TO GFCT0L-TPOMSG-WEB
                                          WRK-FUNCAO
                   MOVE ZEROS          TO GFCT0L-NRO-MSGI
                   MOVE 'S'            TO GFCTY9-FIM
               END-IF
           END-IF.

           IF  GFCT0L-TPOMSG-WEB       EQUAL WRK-RSTA OR WRK-MSGF
               MOVE ZEROS              TO GFCT0L-NRO-MSGI

           ELSE
               ADD 1                   TO GFCT0L-NRO-MSGI
           END-IF.

           IF (WRK-FUNCAO              EQUAL  WRK-RSTA)
               PERFORM 1100-RETORNO-GFCT5574
           END-IF.

           PERFORM 0600-CHAMAR-SBAT3000.

      *----------------------------------------------------------------*
       1000-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *    ROTINA PARA RETORNO DO MODULO GFCT5574                      *
      *----------------------------------------------------------------*
       1100-RETORNO-GFCT5574           SECTION.
      *----------------------------------------------------------------*

           MOVE LENGTH  OF GFCTY8-ENTRADA

                                       TO GFCT3O-LL.
           MOVE GFCTY9-FUNCAO          TO GFCT3O-FUNCAO.
           MOVE GFCTY9-FILTRO          TO GFCTY8-FILTRO.
           MOVE GFCTY9-PONTEIRO        TO GFCTY8-PONTEIRO.
           MOVE GFCTY9-QTDE-OCOR       TO GFCTY8-QTDE-OCOR.
           MOVE ZEROS                  TO GFCTY8-QTDE-TOT-REG.
           MOVE GFCTY9-FUNC-BDSCO      TO GFCTY8-FUNC-BDSCO.
           MOVE GFCTY9-FIM             TO GFCTY8-FIM.
           MOVE GFCTY8-DADOS-RST       TO GFCT3O-DADOS-RST.

      *----------------------------------------------------------------*
       1100-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *    ROTINA PARA TRATAR TARIFA AGRUPAMENTO                       *
      *----------------------------------------------------------------*
       1200-TRATAR-COMB-TARIF-AGR      SECTION.
      *----------------------------------------------------------------*

           IF  GFCTYC-FIM              EQUAL 'N'

               MOVE SPACES             TO GFCTYD-SAIDA
                                          GFCT0M-AREA-ERROS
               INITIALIZE GFCT0L-RET-WEB
                          GFCTYD-SAIDA
                          GFCT0M-AREA-ERROS
           END-IF.

           PERFORM 1300-CHAMAR-GFCT5575.

      *----------------------------------------------------------------*
       1200-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *    ROTINA PARA CHAMAR MODULO GFCT5575                          *
      *----------------------------------------------------------------*
       1300-CHAMAR-GFCT5575            SECTION.
      *----------------------------------------------------------------*

           MOVE GFCTYC-FUNCAO          TO WRK-MODULO.


           CALL 'POOL0081'             USING WRK-MODULO
                                             GFCTYC-ENTRADA
                                             GFCTYD-SAIDA
                                             GFCT0M-AREA-ERROS.

           IF (RETURN-CODE             NOT EQUAL ZEROS)
               PERFORM 9997-TRATAR-ERRO-MODULO
           END-IF.

           IF  GFCTYD-ERRO             EQUAL 9
               PERFORM 9998-TRATAR-ERRO-APL-DB2
           END-IF.

           MOVE GFCTYD-SAIDA           TO GFCT0L-ENTRADA.
           MOVE '00'                   TO GFCT0L-MPARCODE-APLIC.

           IF (GFCTYD-FIM              EQUAL 'S') AND
              (IND-1                   LESS   3 )
               MOVE WRK-MSGF           TO GFCT0L-TPOMSG-WEB
                                          WRK-FUNCAO
               MOVE ZEROS              TO GFCT0L-NRO-MSGI

           ELSE
               IF (IND-1               LESS 3)
                   MOVE WRK-MSGI       TO GFCT0L-TPOMSG-WEB
                                          WRK-FUNCAO
                   MOVE GFCTYD-FILTRO  TO GFCTYC-FILTRO
                   MOVE GFCTYD-PONTEIRO
                                       TO GFCTYC-PONTEIRO
               ELSE
                   MOVE WRK-RSTA       TO GFCT0L-TPOMSG-WEB
                                          WRK-FUNCAO
                   MOVE ZEROS          TO GFCT0L-NRO-MSGI
                   MOVE 'S'            TO GFCTYD-FIM
               END-IF
           END-IF.

           IF  GFCT0L-TPOMSG-WEB       EQUAL WRK-RSTA OR WRK-MSGF
               MOVE ZEROS              TO GFCT0L-NRO-MSGI
           ELSE
               ADD 1                   TO GFCT0L-NRO-MSGI
           END-IF.


           IF (WRK-FUNCAO              EQUAL  WRK-RSTA)
               PERFORM 1400-RETORNO-GFCT5575
           END-IF.

           PERFORM 0600-CHAMAR-SBAT3000.

      *----------------------------------------------------------------*
       1300-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *    ROTINA PARA RETORNO DO MODULO GFCT5575                      *
      *----------------------------------------------------------------*
       1400-RETORNO-GFCT5575           SECTION.
      *----------------------------------------------------------------*

           MOVE LENGTH  OF GFCTYC-ENTRADA
                                       TO GFCT3O-LL.
           MOVE GFCTYD-FUNCAO          TO GFCT3O-FUNCAO.
           MOVE GFCTYD-FILTRO          TO GFCTYC-FILTRO.
           MOVE GFCTYD-PONTEIRO        TO GFCTYC-PONTEIRO.

           MOVE GFCTYD-QTDE-OCOR       TO GFCTYC-QTDE-OCOR.
           MOVE ZEROS                  TO GFCTYC-QTDE-TOT-REG.
           MOVE GFCTYD-FUNC-BDSCO      TO GFCTYC-FUNC-BDSCO.
           MOVE GFCTYD-FIM             TO GFCTYC-FIM.
           MOVE GFCTYC-DADOS-RST       TO GFCT3O-DADOS-RST.

      *----------------------------------------------------------------*
       1400-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *    ROTINA PARA TRATAR CANCELAMENTO                             *
      *----------------------------------------------------------------*
       1500-TRATAR-CANCELAMENTO        SECTION.
      *----------------------------------------------------------------*

           MOVE SPACES                 TO GFCTWF-SAIDA
                                          GFCT0M-AREA-ERROS.

           INITIALIZE GFCT0L-RET-WEB
                      GFCTWF-SAIDA

                      GFCT0M-AREA-ERROS.

           PERFORM 1510-CHAMAR-GFCT0959.

      *----------------------------------------------------------------*
       1500-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *    ROTINA PARA CHAMAR MODULO GFCT0959                          *
      *----------------------------------------------------------------*
       1510-CHAMAR-GFCT0959            SECTION.
      *----------------------------------------------------------------*

           MOVE GFCTWE-FUNCAO          TO WRK-MODULO.

           CALL 'POOL0081'             USING WRK-MODULO
                                             GFCTWE-ENTRADA
                                             GFCTWF-SAIDA
                                             GFCT0M-AREA-ERROS.


           IF  RETURN-CODE             NOT EQUAL ZEROS
               PERFORM 9997-TRATAR-ERRO-MODULO
           END-IF.

           IF  GFCTWF-ERRO             EQUAL 9
               PERFORM 9998-TRATAR-ERRO-APL-DB2
           END-IF.

           MOVE GFCTWF-SAIDA           TO GFCT0L-ENTRADA.

           IF  GFCTWF-FIM              EQUAL 'S'
               MOVE WRK-MSGF           TO GFCT0L-TPOMSG-WEB
                                          WRK-FUNCAO
               MOVE ZEROS              TO GFCT0L-NRO-MSGI
           ELSE
               MOVE WRK-MSGI           TO GFCT0L-TPOMSG-WEB
                                          WRK-FUNCAO
               ADD  1                  TO GFCT0L-NRO-MSGI
           END-IF.

           PERFORM 0600-CHAMAR-SBAT3000.


      *----------------------------------------------------------------*
       1510-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *    ROTINA PARA TRATAR CONSULTA SOLICITACAO TARIFAS SUSPENSAS   *
      *----------------------------------------------------------------*
       1600-TRATAR-CONS-TARIFAS-SUSP   SECTION.
      *----------------------------------------------------------------*

           IF  GFCTWG-FIM              EQUAL 'N'
               MOVE SPACES             TO GFCTWH-SAIDA
                                          GFCT0M-AREA-ERROS
               INITIALIZE GFCT0L-RET-WEB
                          GFCTWH-SAIDA
                          GFCT0M-AREA-ERROS
           END-IF.

           PERFORM 1610-CHAMA-GFCT0960.


      *----------------------------------------------------------------*
       1600-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *    ROTINA PARA CHAMAR O MODULO GFCT0960                        *
      *----------------------------------------------------------------*
       1610-CHAMA-GFCT0960             SECTION.
      *----------------------------------------------------------------*

           MOVE GFCTWG-FUNCAO          TO WRK-MODULO.

           CALL 'POOL0081'             USING WRK-MODULO
                                             GFCTWG-ENTRADA
                                             GFCTWH-SAIDA
                                             GFCT0M-AREA-ERROS.

           IF  RETURN-CODE             NOT EQUAL ZEROS
               PERFORM 9997-TRATAR-ERRO-MODULO
           END-IF.


           IF  GFCTWH-ERRO             EQUAL 9
               PERFORM 9998-TRATAR-ERRO-APL-DB2
           END-IF.

           MOVE GFCTWH-SAIDA           TO GFCT0L-ENTRADA.

           IF (GFCTWH-FIM              EQUAL 'S')
               MOVE WRK-MSGF           TO GFCT0L-TPOMSG-WEB
                                          WRK-FUNCAO
               MOVE ZEROS              TO GFCT0L-NRO-MSGI
           ELSE
               IF (IND-1               LESS 3)
                   MOVE WRK-MSGI       TO GFCT0L-TPOMSG-WEB
                                          WRK-FUNCAO
                   MOVE GFCTWH-FILTRO  TO GFCTWG-FILTRO
                   MOVE GFCTWH-QTDE-OCOR
                                       TO GFCTWG-QTDE-OCOR
                   MOVE ZEROS          TO GFCTWG-QTDE-TOT-REG
               ELSE
                   MOVE WRK-RSTA       TO GFCT0L-TPOMSG-WEB
                                          WRK-FUNCAO

                   MOVE ZEROS          TO GFCT0L-NRO-MSGI
                   MOVE 'S'            TO GFCTY1-FIM
               END-IF
           END-IF.

           IF  GFCT0L-TPOMSG-WEB       EQUAL  WRK-RSTA OR WRK-MSGF
               MOVE ZEROS              TO GFCT0L-NRO-MSGI
           ELSE
               ADD 1                   TO GFCT0L-NRO-MSGI
           END-IF.

           IF  WRK-FUNCAO              EQUAL WRK-RSTA
               PERFORM 1611-RETORNO-GFCT0960
           END-IF.

           PERFORM 0600-CHAMAR-SBAT3000.

      *----------------------------------------------------------------*
       1610-99-FIM.                    EXIT.
      *----------------------------------------------------------------*
      *----------------------------------------------------------------*
      *    ROTINA PARA RETORNO DO MODULO GFCT0960                      *
      *----------------------------------------------------------------*
       1611-RETORNO-GFCT0960           SECTION.
      *----------------------------------------------------------------*

           MOVE +256                   TO GFCT3O-LL.
           MOVE GFCTWH-FUNCAO          TO GFCT3O-FUNCAO.
           MOVE GFCTWH-FILTRO          TO GFCTWG-FILTRO.
           MOVE GFCTWH-QTDE-OCOR       TO GFCTWG-QTDE-OCOR.
           MOVE ZEROS                  TO GFCTWG-QTDE-TOT-REG.
           MOVE GFCTWH-FUNC-BDSCO      TO GFCTWG-FUNC-BDSCO.
           MOVE GFCTWH-FIM             TO GFCTWG-FIM.
           MOVE GFCTWG-DADOS-RST       TO GFCT3O-DADOS-RST.

      *----------------------------------------------------------------*
       1611-99-FIM.                    EXIT.
      *----------------------------------------------------------------*
      *----------------------------------------------------------------*
      *    ROTINA PARA TRATAR ERRO DE MODULO                           *
      *----------------------------------------------------------------*
       9997-TRATAR-ERRO-MODULO         SECTION.
      *----------------------------------------------------------------*

           MOVE 'APL'                  TO GFCT0M-TIPO-ACESSO.
           MOVE WRK-MODULO             TO WRK-MODULO-MSG05
                                          GFCT0M-PROGRAMA.
           MOVE WRK-MSG05              TO GFCT0M-TEXTO

           PERFORM 9999-PROCESSAR-ROTINA-ERRO.

      *----------------------------------------------------------------*
       9997-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *    ROTINA PARA ERROS APL DB2                                   *
      *----------------------------------------------------------------*
       9998-TRATAR-ERRO-APL-DB2        SECTION.
      *----------------------------------------------------------------*


           IF  GFCT0M-TIPO-ACESSO      NOT EQUAL 'DB2'
               MOVE 'APL'              TO GFCT0M-TIPO-ACESSO
               MOVE WRK-MODULO         TO GFCT0M-PROGRAMA
           END-IF.

           PERFORM 9999-PROCESSAR-ROTINA-ERRO.

      *----------------------------------------------------------------*
       9998-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *    ROTINA PARA ERROS                                           *
      *----------------------------------------------------------------*
       9999-PROCESSAR-ROTINA-ERRO      SECTION.
      *----------------------------------------------------------------*

           MOVE '02'                   TO GFCT0L-MPARCODE-APLIC.
           MOVE 'GFCT7940'             TO GFCT0M-TRANSACAO.
           MOVE GFCT0L-MPARCODE-APLIC  TO MPA-RCODE-APLICACAO.
           MOVE WRK-ERRO               TO WRK-FUNCAO.


           IF  GFCT0M-SQLCA-AREA       EQUAL SPACES OR LOW-VALUES
               CALL 'SBAT3000'         USING WRK-FUNCAO
                                             LNK-IO-PCB
                                             LNK-ALT-PCB
                                             MCA-ISDPA
                                             GFCT0M-ERRO-AREAS
           ELSE
               CALL 'SBAT3000'         USING WRK-FUNCAO
                                             LNK-IO-PCB
                                             LNK-ALT-PCB
                                             MCA-ISDPA
                                             GFCT0M-ERRO-AREAS
                                             GFCT0M-SQLCA-AREA
           END-IF.

           GOBACK.

      *----------------------------------------------------------------*
       9999-99-FIM.                    EXIT.
      *----------------------------------------------------------------*