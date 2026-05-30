      *================================================================*
       IDENTIFICATION                  DIVISION.
      *================================================================*

       PROGRAM-ID. GFCT7542.
       AUTHOR.     HELIO SANTONI NETO.
      *================================================================*
      *    S O N D A   P R O C W O R K  -  C O N S U L T O R I A       *
      *----------------------------------------------------------------*
      *                                                                *
      *    PROGRAMA     : GFCT7542                                     *
      *    PROGRAMADOR  : HELIO SANTONI           - PROCWORK  / GP.50  *
      *    ANALISTA     : VALERIA TORQUATO        - PROCWORK  / GP.50  *
      *    DATA         : 06/12/2008                                   *
      *                                                                *
      *    OBJETIVO     :                                              *
      *      ROTEADOR DE SOLICITACAO DE PREVIA PARA VALIDACAO DA       *
      *      EMISS O DE EXTRATOS VIA CORREIO.                          *
      *                                                                *
      *    MODULOS CHAMADOS:                                           *
      *      SBAT3000 - MODULO DE COMUNICACAO ISD.                     *
      *      GFCT0543 - INCLUSAO.                                      *
      *      GFCT0544 - CONSULTA.                                      *
      *      GFCT5545 - COMBO DE ANO.                                  *
      *      GFCT5546 - COMBO DE DATA / HORA.                          *
      *      GFCT5006 - LUPA  DE PESQUISA NOME AGENCIA.                *
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
           '*  INICIO DA WORKING GFCT7542  *'.
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

       01  IND-INDEXADORES.
           05  IND-1                   PIC 9(03) COMP-3    VALUE ZEROS.

      *----------------------------------------------------------------*
       01  FILLER                      PIC X(32)           VALUE
           '*     VARIAVEIS AUXILIARES     *'.
      *----------------------------------------------------------------*

       77  WRK-CALC-02                 PIC 9(02) COMP-3    VALUE ZEROS.

       01  WRK-AUXILIARES.
           05  WRK-FUNCAO              PIC X(04)           VALUE SPACES.
           05  WRK-MODULO              PIC X(08)           VALUE SPACES.
           05  WRK-NRO-MSGI            PIC 9(02) COMP-3    VALUE ZEROS.
           05  WRK-X-100.
             10  WRK-X-100-R           PIC X(100)          VALUE SPACES.
             10  FILLER                PIC X(910)          VALUE SPACES.
           05  WRK-X-680.
             10  WRK-X-680-R           PIC X(680)          VALUE SPACES.
             10  FILLER                PIC X(330)          VALUE SPACES.
          05 WRK-BLOCO                 PIC 9(03) COMP-3    VALUE ZEROS.

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

       COPY I#ISDDPC.
       COPY I#GFCT0K.
       COPY I#GFCT0L.
       COPY I#GFCT0M.
       COPY I#GFCT3O.
       COPY I#GFCT1V.
       COPY I#GFCT1X.
       COPY GFCTWADA.
       COPY GFCTWADB.
       COPY GFCTWAC8.
       COPY GFCTWAC9.
       COPY GFCTWAC6.
       COPY GFCTWAC7.
       COPY GFCTWADC.
       COPY GFCTWADD.

      *----------------------------------------------------------------*
       01  FILLER                      PIC X(32)           VALUE
           '*    FIM DA WORKING GFCT7542   *'.
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
      *    ROTINA INICIAL                                              *
      *----------------------------------------------------------------*
       0000-INICIAR                    SECTION.
      *----------------------------------------------------------------*

           MOVE SPACES                 TO GFCT0M-AREA-ERROS.

           INITIALIZE                  GFCT0K-AREA
                                       GFCT0L-AREA
                                       GFCT0M-AREA-ERROS
                                       GFCT3O-AREA-RESTART
                                       GFCT1V-ENTRADA
                                       GFCT1X-SAIDA
                                       GFCTWAC6-ENTRADA
                                       GFCTWAC7-SAIDA
                                       GFCTWADC-ENTRADA
                                       GFCTWADD-SAIDA
                                       GFCTWAC8-ENTRADA
                                       GFCTWAC9-SAIDA
                                       GFCTWADA-ENTRADA
                                       GFCTWADB-SAIDA.

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

               WHEN 'GFCT0543'
                   MOVE GFCT0K-ENTRADA TO WRK-X-100
                   MOVE WRK-X-100-R    TO GFCTWAC6-ENTRADA
                   PERFORM 2100-PROCESSAR-INCLUSAO

               WHEN 'GFCT0544'
                   MOVE GFCT0K-ENTRADA TO WRK-X-100
                   MOVE WRK-X-100-R    TO GFCTWADC-ENTRADA
                   PERFORM 2200-PROCESSO-CONSULTA
                                VARYING IND-1  FROM 1 BY 1
                                  UNTIL IND-1  GREATER 3
                                     OR GFCTWADD-FIM  EQUAL 'S'

               WHEN 'GFCT5545'
                   MOVE GFCT0K-ENTRADA TO WRK-X-100
                   MOVE WRK-X-100-R    TO GFCTWAC8-ENTRADA
                   PERFORM 2300-PROCESSAR-COMBO-ANO
                                VARYING IND-1  FROM 1 BY 1
                                  UNTIL IND-1  GREATER 3
                                     OR GFCTWAC9-FIM  EQUAL 'S'

               WHEN 'GFCT5546'
                   MOVE GFCT0K-ENTRADA TO WRK-X-100
                   MOVE WRK-X-100-R    TO GFCTWADA-ENTRADA
                   PERFORM 2400-PROCESSAR-COMB-DATAHORA
                                VARYING IND-1  FROM 1 BY 1
                                  UNTIL IND-1  GREATER 3
                                     OR GFCTWADB-FIM  EQUAL 'S'

               WHEN 'GFCT5006'
                   MOVE GFCT0K-ENTRADA TO WRK-X-680
                   MOVE WRK-X-680-R    TO GFCT1V-ENTRADA
                   PERFORM 2500-OBTER-NOME-DEPTO

           END-EVALUATE.

      *----------------------------------------------------------------*
       2000-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *    ROTINA PARA PROCESSAR INCLUSAO                              *
      *----------------------------------------------------------------*
       2100-PROCESSAR-INCLUSAO         SECTION.
      *----------------------------------------------------------------*

           MOVE SPACES                 TO GFCTWAC7-SAIDA

           INITIALIZE GFCTWAC7-SAIDA

           PERFORM 2110-CHAMAR-GFCT0543

           MOVE GFCTWAC7-SAIDA         TO GFCT0L-ENTRADA.

           PERFORM 2120-TRATAR-RETORNO.

           PERFORM 2130-CHAMAR-SBAT3000.

      *----------------------------------------------------------------*
       2100-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *    ROTINA PARA ACESSAR O MODULO GFCT0543                       *
      *----------------------------------------------------------------*
       2110-CHAMAR-GFCT0543            SECTION.
      *----------------------------------------------------------------*

           MOVE '00'                   TO GFCT0L-MPARCODE-APLIC.
           MOVE GFCTWAC6-FUNCAO        TO WRK-MODULO.

           CALL WRK-MODULO             USING GFCTWAC6-ENTRADA
                                             GFCTWAC7-SAIDA
                                             GFCT0M-AREA-ERROS.

           IF  RETURN-CODE             NOT EQUAL ZEROS
               MOVE ZEROS              TO RETURN-CODE
               MOVE WRK-MODULO         TO WRK-MODULO-MSG
               MOVE WRK-MSG005         TO GFCT0M-TEXTO(1:34)
               PERFORM 9998-FORMATAR-ERRO
               PERFORM 9999-TRATAR-ERRO
           END-IF.

           IF  GFCTWAC7-ERRO           EQUAL 9
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
      *    ROTINA PARA PROCESSAR CONSULTA                              *
      *----------------------------------------------------------------*
       2200-PROCESSO-CONSULTA          SECTION.
      *----------------------------------------------------------------*

           IF  GFCTWADD-FIM            EQUAL 'N'
               MOVE SPACES             TO GFCTWADD-SAIDA
                                          GFCT0M-AREA-ERROS
                                          GFCT3O-AREA-RESTART
               INITIALIZE                 GFCT3O-AREA-RESTART
                                          GFCTWADD-SAIDA
                                          GFCT0M-AREA-ERROS
           END-IF.

           PERFORM 2210-CHAMAR-GFCT0544.

           MOVE GFCTWADD-SAIDA         TO GFCT0L-ENTRADA.

           PERFORM 2220-TRATAR-RET-CONS

           PERFORM 2230-CHAMAR-SBAT3000.

      *----------------------------------------------------------------*
       2200-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *    ROTINA PARA ACESSAR MODULO GFCT0544                         *
      *----------------------------------------------------------------*
       2210-CHAMAR-GFCT0544            SECTION.
      *----------------------------------------------------------------*

           MOVE '00'                   TO GFCT0L-MPARCODE-APLIC.
           MOVE GFCTWADC-FUNCAO        TO WRK-MODULO.

           CALL WRK-MODULO             USING GFCTWADC-ENTRADA
                                             GFCTWADD-SAIDA
                                             GFCT0M-AREA-ERROS.

           IF  RETURN-CODE             NOT EQUAL ZEROS
               MOVE ZEROS              TO RETURN-CODE
               MOVE WRK-MODULO         TO WRK-MODULO-MSG
               MOVE WRK-MSG005         TO GFCT0M-TEXTO(01:34)
               PERFORM 9998-FORMATAR-ERRO
               PERFORM 9999-TRATAR-ERRO
           END-IF.

           IF  GFCTWADD-ERRO           EQUAL 9
               PERFORM 9999-TRATAR-ERRO
           END-IF.

      *----------------------------------------------------------------*
       2210-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *    ROTINA PARA TRATAR RETORNO CONSULTA                         *
      *----------------------------------------------------------------*
       2220-TRATAR-RET-CONS            SECTION.
      *----------------------------------------------------------------*

           IF  GFCTWADD-FIM         EQUAL 'S'
               MOVE WRK-MSGF           TO GFCT0L-TPOMSG-WEB
           ELSE
               IF  IND-1             LESS 3
                   MOVE WRK-MSGI       TO GFCT0L-TPOMSG-WEB
                   MOVE GFCTWADD-QTDE-OCOR
                                       TO GFCTWADC-QTDE-OCOR
                   MOVE GFCTWADD-FILTRO
                                       TO GFCTWADC-FILTRO
               ELSE
                   MOVE WRK-RSTA       TO GFCT0L-TPOMSG-WEB
                   PERFORM 2221-MOVER-CAMPOS-01
               END-IF
           END-IF.

           IF  GFCT0L-TPOMSG-WEB       EQUAL WRK-RSTA OR WRK-MSGF
               MOVE ZEROS              TO GFCT0L-NRO-MSGI
           ELSE
               MOVE GFCT0L-NRO-MSGI    TO WRK-CALC-02
               ADD 1                   TO WRK-CALC-02
               MOVE WRK-CALC-02        TO GFCT0L-NRO-MSGI
           END-IF.

           MOVE '00'                   TO GFCT0L-MPARCODE-APLIC
           MOVE GFCT0L-TPOMSG-WEB      TO WRK-FUNCAO.
           MOVE GFCTWADD-SAIDA         TO GFCT0L-ENTRADA.

      *----------------------------------------------------------------*
       2220-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *    MOVIMENTACAO DE CAMPOS.                                     *
      ******************************************************************
      *----------------------------------------------------------------*
       2221-MOVER-CAMPOS-01            SECTION.
      *----------------------------------------------------------------*

           MOVE 255                    TO GFCT3O-LL.
           MOVE GFCTWADD-FUNCAO        TO GFCT3O-FUNCAO.
           MOVE GFCTWADD-QTDE-OCOR     TO GFCTWADC-QTDE-OCOR.
           MOVE ZEROS                  TO GFCTWADC-QTDE-TOT-REG.
           MOVE GFCTWADD-FUNC-BDSCO    TO GFCTWADC-FUNC-BDSCO.
           MOVE GFCTWADD-FILTRO        TO GFCTWADC-FILTRO.
           MOVE GFCTWADD-FIM           TO GFCTWADC-FIM.
           MOVE GFCTWADC-DADOS-RST     TO GFCT3O-DADOS-RST.

      *----------------------------------------------------------------*
       2221-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *    ROTINA PARA RETORNO DA LISTA DE CONSULTA                    *
      *----------------------------------------------------------------*
       2230-CHAMAR-SBAT3000         SECTION.
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
       2230-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *    ROTINA PARA PROCESSAR COMBO DE ANO                          *
      *----------------------------------------------------------------*
       2300-PROCESSAR-COMBO-ANO        SECTION.
      *----------------------------------------------------------------*

           IF  GFCTWAC8-FIM            EQUAL 'N'
               MOVE SPACES             TO GFCTWAC9-SAIDA
                                          GFCT0M-AREA-ERROS
               INITIALIZE                 GFCT3O-AREA-RESTART
                                          GFCTWAC9-SAIDA
                                          GFCT0M-AREA-ERROS
           END-IF.

           PERFORM 2310-CHAMAR-GFCT5545.

           MOVE GFCTWAC9-SAIDA         TO GFCT0L-ENTRADA.

           PERFORM 2320-TRATAR-RET-ANO

           PERFORM 2330-RETORNAR-ANO.

      *----------------------------------------------------------------*
       2300-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *    ROTINA PARA ACESSAR MODULO GFCT5545                         *
      *----------------------------------------------------------------*
       2310-CHAMAR-GFCT5545     SECTION.
      *----------------------------------------------------------------*

           MOVE '00'                   TO GFCT0L-MPARCODE-APLIC.
           MOVE GFCTWAC8-FUNCAO        TO WRK-MODULO.

           CALL WRK-MODULO             USING GFCTWAC8-ENTRADA
                                             GFCTWAC9-SAIDA
                                             GFCT0M-AREA-ERROS.

           IF  RETURN-CODE             NOT EQUAL ZEROS
               MOVE ZEROS              TO RETURN-CODE
               MOVE WRK-MODULO         TO WRK-MODULO-MSG
               MOVE WRK-MSG005         TO GFCT0M-TEXTO(01:34)
               PERFORM 9998-FORMATAR-ERRO
               PERFORM 9999-TRATAR-ERRO
           END-IF.

           IF  GFCTWAC9-ERRO           EQUAL 9
               PERFORM 9999-TRATAR-ERRO
           END-IF.

      *----------------------------------------------------------------*
       2310-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *    ROTINA PARA TRATAR RETORNO ANO                              *
      *----------------------------------------------------------------*
       2320-TRATAR-RET-ANO             SECTION.
      *----------------------------------------------------------------*

           IF  GFCTWAC9-FIM         EQUAL 'S'
               MOVE WRK-MSGF           TO GFCT0L-TPOMSG-WEB
               MOVE ZEROS              TO GFCT0L-NRO-MSGI
           ELSE
               MOVE GFCT0L-NRO-MSGI    TO WRK-NRO-MSGI
               ADD 1                   TO WRK-BLOCO
                                          WRK-NRO-MSGI
               MOVE WRK-NRO-MSGI       TO GFCT0L-NRO-MSGI
               MOVE GFCTWAC9-PONTEIRO  TO GFCTWAC8-PONTEIRO
               MOVE GFCTWAC9-QTDE-TOT-REG
                                       TO GFCTWAC8-QTDE-TOT-REG
               MOVE GFCTWAC9-FIM       TO GFCTWAC8-FIM
               MOVE WRK-MSGI           TO GFCT0L-TPOMSG-WEB
               IF  WRK-BLOCO           EQUAL 3
                   MOVE WRK-RSTA       TO GFCT0L-TPOMSG-WEB
                   MOVE ZEROS          TO GFCT0L-NRO-MSGI
               END-IF
           END-IF.

           MOVE '00'                   TO GFCT0L-MPARCODE-APLIC
           MOVE GFCT0L-TPOMSG-WEB      TO WRK-FUNCAO.

      *----------------------------------------------------------------*
       2320-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *    ROTINA PARA RETORNO DE AGRUPAMENTO                          *
      *----------------------------------------------------------------*
       2330-RETORNAR-ANO               SECTION.
      *----------------------------------------------------------------*

           MOVE GFCTWAC9-SAIDA         TO GFCT0L-ENTRADA.

           IF  WRK-FUNCAO              EQUAL 'RSTA'
               MOVE 255                TO GFCT3O-LL
               MOVE GFCTWAC9-FUNCAO    TO GFCT3O-FUNCAO
               MOVE GFCTWAC9-FILTRO    TO GFCTWAC8-FILTRO
               MOVE GFCTWAC9-QTDE-OCOR TO GFCTWAC8-QTDE-OCOR
               MOVE ZEROS              TO GFCTWAC8-QTDE-TOT-REG
               MOVE GFCTWAC9-FUNC-BDSCO
                                       TO GFCTWAC8-FUNC-BDSCO
               MOVE GFCTWAC9-FIM       TO GFCTWAC8-FIM
               MOVE GFCTWAC8-DADOS-RST TO GFCT3O-DADOS-RST
           END-IF.

           PERFORM 2130-CHAMAR-SBAT3000.

      *----------------------------------------------------------------*
       2330-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *    ROTINA PARA ACESSAR COMBO DATA HORA                         *
      *----------------------------------------------------------------*
       2400-PROCESSAR-COMB-DATAHORA    SECTION.
      *----------------------------------------------------------------*

           IF  GFCTWADA-FIM            EQUAL 'N'
               MOVE SPACES             TO GFCTWADB-SAIDA
                                          GFCT0M-AREA-ERROS
               INITIALIZE                 GFCT3O-AREA-RESTART
                                          GFCTWADB-SAIDA
                                          GFCT0M-AREA-ERROS
           END-IF.

           PERFORM 2410-CHAMAR-GFCT5546

           MOVE GFCTWADB-SAIDA         TO GFCT0L-ENTRADA.

           PERFORM 2420-TRATAR-DATA-HORA

           PERFORM 2430-RETORNAR-DATA-HORA.

      *----------------------------------------------------------------*
       2400-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *    ROTINA PARA ACESSAR MODULO GFCT5546                         *
      *----------------------------------------------------------------*
       2410-CHAMAR-GFCT5546            SECTION.
      *----------------------------------------------------------------*

           MOVE '00'                   TO GFCT0L-MPARCODE-APLIC.
           MOVE GFCTWADA-FUNCAO        TO WRK-MODULO.

           CALL WRK-MODULO             USING GFCTWADA-ENTRADA
                                             GFCTWADB-SAIDA
                                             GFCT0M-AREA-ERROS.

           IF  RETURN-CODE             NOT EQUAL ZEROS
               MOVE ZEROS              TO RETURN-CODE
               MOVE WRK-MODULO         TO WRK-MODULO-MSG
               MOVE WRK-MSG005         TO GFCT0M-TEXTO(01:34)
               PERFORM 9998-FORMATAR-ERRO
               PERFORM 9999-TRATAR-ERRO
           END-IF.

           IF  GFCTWADB-ERRO           EQUAL 9
               PERFORM 9999-TRATAR-ERRO
           END-IF.

      *----------------------------------------------------------------*
       2410-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *    ROTINA PARA TRATAR COMBO DATA HORA                          *
      *----------------------------------------------------------------*
       2420-TRATAR-DATA-HORA           SECTION.
      *----------------------------------------------------------------*

           IF  GFCTWADB-FIM              EQUAL 'S'
               MOVE WRK-MSGF           TO GFCT0L-TPOMSG-WEB
               MOVE ZEROS              TO GFCT0L-NRO-MSGI
           ELSE
               MOVE GFCT0L-NRO-MSGI    TO WRK-NRO-MSGI
               ADD 1                   TO WRK-BLOCO
                                          WRK-NRO-MSGI
               MOVE WRK-NRO-MSGI       TO GFCT0L-NRO-MSGI
               MOVE GFCTWADB-QTDE-TOT-REG
                                       TO GFCTWADA-QTDE-TOT-REG
               MOVE GFCTWADB-PONTEIRO  TO GFCTWADA-PONTEIRO
               MOVE GFCTWADB-FIM       TO GFCTWADA-FIM
               MOVE WRK-MSGI           TO GFCT0L-TPOMSG-WEB
               IF  WRK-BLOCO            EQUAL 3
                   MOVE WRK-RSTA       TO GFCT0L-TPOMSG-WEB
                   MOVE ZEROS          TO GFCT0L-NRO-MSGI
               END-IF
           END-IF.

           MOVE '00'                   TO GFCT0L-MPARCODE-APLIC
           MOVE GFCT0L-TPOMSG-WEB      TO WRK-FUNCAO.

      *----------------------------------------------------------------*
       2420-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *    ROTINA PARA RETORNAR COMBO DATA HORA                        *
      *----------------------------------------------------------------*
       2430-RETORNAR-DATA-HORA         SECTION.
      *----------------------------------------------------------------*

           MOVE GFCTWADB-SAIDA         TO GFCT0L-ENTRADA.

           IF  WRK-FUNCAO              EQUAL 'RSTA'
               MOVE 255                TO GFCT3O-LL
               MOVE GFCTWADB-FUNCAO    TO GFCT3O-FUNCAO
               MOVE GFCTWADB-QTDE-OCOR TO GFCTWADA-QTDE-OCOR
               MOVE ZEROS              TO GFCTWADA-QTDE-TOT-REG
               MOVE GFCTWADB-FUNC-BDSCO
                                       TO GFCTWADA-FUNC-BDSCO
               MOVE GFCTWADB-FIM       TO GFCTWADA-FIM
               MOVE GFCTWADB-FILTRO    TO GFCTWADA-FILTRO
               MOVE GFCTWADB-PONTEIRO  TO GFCTWADA-PONTEIRO
               MOVE GFCTWADA-DADOS-RST TO GFCT3O-DADOS-RST
           END-IF.

           PERFORM 2130-CHAMAR-SBAT3000.

      *----------------------------------------------------------------*
       2430-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *    ROTINA PARA OBTER NOME DA AGENCIA                           *
      *----------------------------------------------------------------*
       2500-OBTER-NOME-DEPTO           SECTION.
      *----------------------------------------------------------------*

           IF  GFCT1V-FIM              EQUAL 'N'
               MOVE SPACES             TO GFCT1X-SAIDA
                                          GFCT0M-AREA-ERROS
               INITIALIZE                 GFCT0L-RET-WEB
                                          GFCT1X-SAIDA
                                          GFCT0M-AREA-ERROS
           END-IF.

           MOVE '00'                   TO GFCT0L-MPARCODE-APLIC.
           MOVE GFCT1V-FUNCAO          TO WRK-MODULO.

           CALL WRK-MODULO          USING GFCT1V-ENTRADA
                                          GFCT1X-SAIDA
                                          GFCT0M-AREA-ERROS.

           IF  RETURN-CODE             NOT EQUAL ZEROS
               MOVE ZEROS              TO RETURN-CODE
               MOVE WRK-MODULO         TO WRK-MODULO-MSG
               MOVE WRK-MSG005         TO GFCT0M-TEXTO(01:34)
               PERFORM 9998-FORMATAR-ERRO
               PERFORM 9999-TRATAR-ERRO
           END-IF.

           IF  GFCT1X-ERRO             EQUAL 9
               PERFORM 9999-TRATAR-ERRO
           END-IF.

           MOVE GFCT1X-SAIDA           TO GFCT0L-ENTRADA.

           PERFORM 2120-TRATAR-RETORNO.

           PERFORM 2130-CHAMAR-SBAT3000.

      *----------------------------------------------------------------*
       2500-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *    FORMATAR ERRO                                               *
      *----------------------------------------------------------------*
       9998-FORMATAR-ERRO              SECTION.
      *----------------------------------------------------------------*

           MOVE 'APL'                  TO GFCT0M-TIPO-ACESSO.
           MOVE 'GFCT7542'             TO GFCT0M-TRANSACAO.
           MOVE '02'                   TO GFCT0L-MPARCODE-APLIC.

      *----------------------------------------------------------------*
       9998-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *    TRATAMENTO DE ERRO                                          *
      *----------------------------------------------------------------*
       9999-TRATAR-ERRO                SECTION.
      *----------------------------------------------------------------*

           MOVE 'GFCT7542'             TO GFCT0M-PROGRAMA.
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
