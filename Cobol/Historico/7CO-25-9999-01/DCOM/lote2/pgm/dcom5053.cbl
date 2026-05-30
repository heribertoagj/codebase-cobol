       IDENTIFICATION                       DIVISION.
       PROGRAM-ID.                          DCOM5053.
       AUTHOR.                              DUARTE.
       DATE-WRITTEN.                        26-04-2006.
      * INCLUIR ISENCAO DE ANALISE DE RESTRICAO  CEDENTE SUBPRODUTO
       ENVIRONMENT                          DIVISION.
       CONFIGURATION                        SECTION.
       SPECIAL-NAMES.                       DECIMAL-POINT IS COMMA.
       DATA                                 DIVISION.
       WORKING-STORAGE                      SECTION.
       01            FILLER                 PIC  X(032)
                     VALUE '*** INICIO WORKING DCOM5053 ***'.

       77  WRK-DATA-ATUAL                     PIC  X(010).
       77  WRK-DATA-ENTRADA                   PIC  X(010).
       77  WRK-SEQUENCIA                      PIC  9(003).
       77  WRK-DINIC-VGCIA-ISENC-ENT          PIC  X(010) VALUE SPACES.
       77  WRK-DFIM-VGCIA-ISENC-ENT           PIC  X(010) VALUE SPACES.
       77  WRK-DINIC-VGCIA-ISENC-TAB          PIC  X(010) VALUE SPACES.
       77  WRK-DFIM-VGCIA-ISENC-TAB           PIC  X(010) VALUE SPACES.
       77  WRK-ENTRE-VIGENCIAS                PIC  X(001) VALUE 'N'.
       77  WRK-DATA-MENOR                     PIC  X(010) VALUE SPACES.
       77  WRK-TRATA-ALTERACAO                PIC  X(001) VALUE SPACES.
       77  IND-LKG                            PIC  9(003) VALUE ZEROS.

       01  WRK-CCHAVE-ELMTO-DESC.
           03  WRK-CCHAVE-ELMTO-DESC-1        PIC  9(003) VALUE ZEROS.
           03  WRK-CCHAVE-ELMTO-DESC-2        PIC  9(003) VALUE ZEROS.
4S2511*    03  WRK-CCHAVE-ELMTO-DESC-3        PIC  9(009) VALUE ZEROS.
4S2511     03  WRK-CCHAVE-ELMTO-DESC-3        PIC  X(009) VALUE SPACES.
           03  WRK-CCHAVE-ELMTO-DESC-5        PIC  X(010) VALUE SPACES.
           03  WRK-CCHAVE-ELMTO-DESC-6        PIC  9(003) VALUE ZEROS.
           03  FILLER                         PIC  X(014) VALUE SPACES.

       01            FILLER                 PIC  X(032)
                     VALUE '*** COPY I#DCOMSG           ***'.

           COPY      'I#DCOMSG'.


       01            FILLER                 PIC  X(032)
                     VALUE '*** COPY I#DCOM48           ***'.

           COPY      'I#DCOM48'.

       01            FILLER                 PIC  X(032)
                     VALUE '*** COPY I#DCOM63           ***'.

           COPY      'I#DCOM63'.

       01            FILLER                 PIC  X(032)
                     VALUE '*** COPY I#DCOMX1           ***'.

           COPY      'I#DCOMX1'.

       01            FILLER                 PIC  X(032)
                     VALUE '*** COPY I#BRAD7C           ***'.

           COPY      'I#BRAD7C'.

       01            FILLER                 PIC  X(032)
                     VALUE '*** AUXILIARES              ***'.

       01            WS-AUX.
           03        WRK-ERRO               PIC S9(004)
                     COMP
                     VALUE +0.
           03        WRK-TEM-PESSOA         PIC  X(001)
                     VALUE SPACES.
           03        WRK-MODULO             PIC  X(008)
                     VALUE SPACES.
           03        WRK-SQLCA              PIC  X(136)
                     VALUE SPACES.
           03        WRK-TIMESTAMP-ATUAL    PIC  X(026)
                     VALUE SPACES.

           03  WRK-ENTRADA.
                05  WRK-COD-RETORNO-E       PIC  X(004).
                05  WRK-MSG-RETORNO-E       PIC  X(079).
                05  WRK-RESTART-E           PIC  9(005).
                05  WRK-FLAG-E              PIC  X(001).
                05  WRK-CFUNC-BDSCO-E       PIC  9(009).
                05  WRK-CTERM-E             PIC  X(008).
                05  WRK-DADOS-E.
                    07  WRK-CELMTO-DESC-COML-E     PIC  9(03).
                    07  WRK-WCHAVE-PRINC-ELMTO-E   PIC  9(09).
                    07  WRK-QDIG-SEQ-ELMTO-E       PIC  9(03).

          03  WRK-SAIDA.
              05  WRK-HEADER-S.
                  07  WRK-COD-RETORNO-S          PIC  X(04).
                  07  WRK-MSG-RETORNO-S          PIC  X(79).
                  07  WRK-RESTART-S              PIC  9(05).
                  07  WRK-FLAG-S                 PIC  X(01).
                  07  WRK-NULT-SEQ-ELMTO-S       PIC  9(15).


       LINKAGE                              SECTION.

       01          ENTRADA-ROTEADOR.
           03      ROTENT-COD-RETORNO              PIC  X(04).
           03      ROTENT-MENSAGEM                 PIC  X(79).
           03      ROTENT-RESTART                  PIC  9(05).
           03      ROTENT-FLAG                     PIC  X(01).
           03      ROTENT-CFUNC-BDSCO              PIC  9(09).
           03      ROTENT-CTERM                    PIC  X(08).
4S2511*    03      ROTENT-CCNPJ-CPF                PIC  9(09).
4S2511     03      ROTENT-CCNPJ-CPF                PIC  X(09).
4S2511*    03      ROTENT-CFLIAL-CNPJ              PIC  9(04).
4S2511     03      ROTENT-CFLIAL-CNPJ              PIC  X(04).
           03      ROTENT-CCTRL-CNPJ-CPF           PIC  9(02).
           03      ROTENT-CTPO-ISENC-DESC          PIC  9(03).
           03      ROTENT-DINIC-VGCIA-ISENC        PIC  X(10).
           03      ROTENT-DFIM-VGCIA-ISENC         PIC  X(10).
           03      ROTENT-CPRODT                   PIC  9(03).
           03      ROTENT-CSPROD-DESC-COML         PIC  9(03).
           03      ROTENT-CINDCD-REST-VLR          PIC  X(01).
           03      ROTENT-VREST-MAX-OPER           PIC  9(15)V99.

       01          SAIDA-ROTEADOR.
             03    ROTSAI-HEADER.
              05   ROTSAI-COD-RETORNO              PIC  X(04).
              05   ROTSAI-MSG-RETORNO              PIC  X(79).
              05   ROTSAI-RESTART                  PIC  9(05).
              05   ROTSAI-FLAG                     PIC  X(01).

             03    ROTSAI-CONSISTENCIA.
              05   ROTSAI-TAMANHO-ERRO             PIC  9(01).
              05   ROTSAI-TABELA-ERROS             OCCURS  08  TIMES.
               07  ROTSAI-CAMPOS-ERRO              PIC  9(01).

       01           LKG-ERRO-AREA           PIC  X(107).
       01           LKG-SQLCA               PIC  X(136).

       PROCEDURE                            DIVISION
                                            USING  ENTRADA-ROTEADOR
                                                   SAIDA-ROTEADOR
                                                   LKG-ERRO-AREA
                                                   LKG-SQLCA.
      *---------------------------------------------------------------
       000-000-INICIO                       SECTION.
      *---------------------------------------------------------------

           INITIALIZE SAIDA-ROTEADOR
                      48-ARGUMENTOS-ENTRADA
                      63-ARGUMENTOS-ENTRADA
                      X1-ARGUMENTOS-ENTRADA
                      LKG-ERRO-AREA
                      LKG-SQLCA
                      WRK-ERRO.

           MOVE 1                 TO ROTSAI-TAMANHO-ERRO.
           MOVE 0000              TO ROTSAI-COD-RETORNO.
           MOVE SG-MENSAGEM(0001) TO ROTSAI-MSG-RETORNO.

      * CONSISTIR ENTRADAS

4S2511     IF  ROTENT-CCNPJ-CPF EQUAL SPACES OR
4S2511         ROTENT-CCNPJ-CPF EQUAL LOW-VALUES
               MOVE  1                 TO ROTSAI-CAMPOS-ERRO(1)
               MOVE '0223'             TO ROTSAI-COD-RETORNO
               MOVE SG-MENSAGEM(223)
                                       TO ROTSAI-MSG-RETORNO
               PERFORM  010-010-FINALIZAR
           END-IF.

           IF  ROTENT-CTPO-ISENC-DESC NOT NUMERIC OR
              (ROTENT-CTPO-ISENC-DESC NOT EQUAL 001 AND 002)
               MOVE  1                 TO ROTSAI-CAMPOS-ERRO(2)
               MOVE '0495'             TO ROTSAI-COD-RETORNO
               MOVE SG-MENSAGEM(495)
                                       TO ROTSAI-MSG-RETORNO
               PERFORM  010-010-FINALIZAR
           END-IF.

           IF  ROTENT-DINIC-VGCIA-ISENC EQUAL SPACES
               MOVE  1                 TO ROTSAI-CAMPOS-ERRO(3)
               MOVE '0651'             TO ROTSAI-COD-RETORNO
               MOVE SG-MENSAGEM(651)
                                       TO ROTSAI-MSG-RETORNO
               PERFORM  010-010-FINALIZAR
           END-IF.

           IF  ROTENT-DFIM-VGCIA-ISENC EQUAL SPACES
               MOVE  1                 TO ROTSAI-CAMPOS-ERRO(4)
               MOVE '0652'             TO ROTSAI-COD-RETORNO
               MOVE SG-MENSAGEM(652)
                                       TO ROTSAI-MSG-RETORNO
               PERFORM  010-010-FINALIZAR
           END-IF.

           IF  ROTENT-CPRODT NOT NUMERIC OR
               ROTENT-CPRODT EQUAL ZEROS
               MOVE  1                 TO ROTSAI-CAMPOS-ERRO(5)
               MOVE '0115'             TO ROTSAI-COD-RETORNO
               MOVE SG-MENSAGEM(115)
                                       TO ROTSAI-MSG-RETORNO
               PERFORM  010-010-FINALIZAR
           END-IF.

           IF  ROTENT-CSPROD-DESC-COML NOT NUMERIC OR
               ROTENT-CSPROD-DESC-COML EQUAL ZEROS
               MOVE  1                 TO ROTSAI-CAMPOS-ERRO(6)
               MOVE '0116'             TO ROTSAI-COD-RETORNO
               MOVE SG-MENSAGEM(116)
                                       TO ROTSAI-MSG-RETORNO
               PERFORM  010-010-FINALIZAR
           END-IF.

           IF  ROTENT-CINDCD-REST-VLR NOT EQUAL 'S' AND 'N'
               MOVE  1                 TO ROTSAI-CAMPOS-ERRO(6)
               MOVE '0850'             TO ROTSAI-COD-RETORNO
               MOVE SG-MENSAGEM(850)
                                       TO ROTSAI-MSG-RETORNO
               PERFORM  010-010-FINALIZAR
           END-IF.

           IF   ROTENT-CINDCD-REST-VLR EQUAL 'S'
           AND (ROTENT-VREST-MAX-OPER  NOT NUMERIC
            OR  ROTENT-VREST-MAX-OPER  EQUAL ZEROS)
               MOVE  1                 TO ROTSAI-CAMPOS-ERRO(6)
               MOVE '0851'             TO ROTSAI-COD-RETORNO
               MOVE SG-MENSAGEM(851)
                                       TO ROTSAI-MSG-RETORNO
               PERFORM  010-010-FINALIZAR
           END-IF.

      * FIM DAS CONSISTENCIAS INICIAIS

           PERFORM  005-005-OBTEM-TIMESTAMP.

           MOVE WRK-TIMESTAMP-ATUAL(1:10)     TO WRK-DATA-ATUAL.

           MOVE ROTENT-DINIC-VGCIA-ISENC(1:2) TO WRK-DATA-ENTRADA(9:2).
           MOVE '-'                           TO WRK-DATA-ENTRADA(8:1).
           MOVE ROTENT-DINIC-VGCIA-ISENC(4:2) TO WRK-DATA-ENTRADA(6:2).
           MOVE '-'                           TO WRK-DATA-ENTRADA(5:1).
           MOVE ROTENT-DINIC-VGCIA-ISENC(7:4) TO WRK-DATA-ENTRADA(1:4).

           IF  WRK-DATA-ENTRADA LESS WRK-DATA-ATUAL
               MOVE  1                 TO ROTSAI-CAMPOS-ERRO(3)
               MOVE '0145'             TO ROTSAI-COD-RETORNO
               MOVE SG-MENSAGEM(145)
                                       TO ROTSAI-MSG-RETORNO
               PERFORM  010-010-FINALIZAR
           END-IF.

           PERFORM  001-010-OBTER-DATA-MENOR.

           PERFORM  005-010-VALIDA-PESSOA.

           PERFORM 010-000-TRATAR-NEGOCIO.

           PERFORM 010-005-GRAVAR-LOG.

           PERFORM  010-010-FINALIZAR.

      *---------------------------------------------------------------
       000-999-FIM.
      *---------------------------------------------------------------

      *---------------------------------------------------------------
       001-010-OBTER-DATA-MENOR             SECTION.
      *---------------------------------------------------------------
      * OBTEM DATA MENOR PARA TRATAMENTO DO LOG

           MOVE  'PARCIAL-05'                TO 63-INSTRUCAO.
4S2511*    MOVE  ROTENT-CCNPJ-CPF            TO 63-CCNPJ-CPF.
4S2511*    MOVE  ROTENT-CFLIAL-CNPJ          TO 63-CFLIAL-CNPJ.
4S2511     MOVE  ROTENT-CCNPJ-CPF            TO 63-CCNPJ-CPF-ST.
4S2511     MOVE  ROTENT-CFLIAL-CNPJ          TO 63-CFLIAL-CNPJ-ST.
4S2511*    MOVE  ROTENT-CCTRL-CNPJ-CPF       TO 63-CCTRL-CNPJ-CPF
4S2511     MOVE  ROTENT-CCTRL-CNPJ-CPF       TO 63-CCTRL-CNPJ-CPF-ST
           MOVE  ROTENT-CTPO-ISENC-DESC      TO 63-CTPO-ISENC-DESC.
           MOVE  ROTENT-CPRODT               TO 63-CPRODT.
           MOVE  ROTENT-CSPROD-DESC-COML     TO 63-CSPROD-DESC-COML.
           MOVE  ZEROS                       TO 63-QTDE-A-DESPREZAR.
           MOVE  'DCOM6815'                  TO WRK-MODULO.

           CALL  WRK-MODULO                    USING
                                               63-ARGUMENTOS-ENTRADA
                                               63-RETORNO
                                               LKG-ERRO-AREA
                                               LKG-SQLCA.

           IF  63-COD-RETORNO      EQUAL 99
               MOVE '0099'         TO  ROTSAI-COD-RETORNO
               MOVE SG-MENSAGEM(099)
                                   TO  ROTSAI-MSG-RETORNO
               PERFORM  010-010-FINALIZAR
           END-IF.

           IF  63-COD-RETORNO      EQUAL ZEROS
               MOVE 'S'            TO  WRK-TRATA-ALTERACAO
               MOVE LENGTH 63-OCORRENCIAS(1)
                                   TO  X1-WREG-LOG-ANTES-LEN
               MOVE 63-OCORRENCIAS(1)
                                   TO  X1-WREG-LOG-ANTES-TEXT
               MOVE 63RT-DINIC-VGCIA-ISENC(1)
                                   TO  WRK-DATA-MENOR
           ELSE
               MOVE 'N'            TO  WRK-TRATA-ALTERACAO
               MOVE ZEROS          TO  X1-WREG-LOG-ANTES-LEN
               MOVE SPACES         TO  X1-WREG-LOG-ANTES-TEXT
           END-IF.

      *---------------------------------------------------------------
       001-010-999-FIM.
      *---------------------------------------------------------------

      *---------------------------------------------------------------
       001-020-OBTER-DATA-VIGENTE           SECTION.
      *---------------------------------------------------------------
      * OBTEM DATA DA ULTIMA ALTERACAO

           MOVE  'PARCIAL-06'                TO 63-INSTRUCAO.
           MOVE  ROTENT-CCNPJ-CPF            TO 63-CCNPJ-CPF.
FERR       MOVE  ROTENT-CFLIAL-CNPJ          TO 63-CFLIAL-CNPJ.
           MOVE  ROTENT-CCTRL-CNPJ-CPF       TO 63-CCTRL-CNPJ-CPF
           MOVE  ROTENT-CTPO-ISENC-DESC      TO 63-CTPO-ISENC-DESC.
           MOVE  ROTENT-CPRODT               TO 63-CPRODT.
           MOVE  ROTENT-CSPROD-DESC-COML     TO 63-CSPROD-DESC-COML.
           MOVE  ZEROS                       TO 63-QTDE-A-DESPREZAR.
           MOVE  'DCOM6815'                  TO WRK-MODULO.

           CALL  WRK-MODULO                    USING
                                               63-ARGUMENTOS-ENTRADA
                                               63-RETORNO
                                               LKG-ERRO-AREA
                                               LKG-SQLCA.

           IF  63-COD-RETORNO      EQUAL 99
               MOVE '0099'         TO  ROTSAI-COD-RETORNO
               MOVE SG-MENSAGEM(099)
                                   TO  ROTSAI-MSG-RETORNO
               PERFORM  010-010-FINALIZAR
           END-IF.

           MOVE LENGTH 63-OCORRENCIAS(1)
                                   TO  X1-WREG-LOG-APOS-LEN
           MOVE 63-OCORRENCIAS(1)  TO  X1-WREG-LOG-APOS-TEXT.

      *---------------------------------------------------------------
       001-010-999-FIM.
      *---------------------------------------------------------------

      *---------------------------------------------------------------
       010-010-FINALIZAR                    SECTION.
      *---------------------------------------------------------------
      * PROCEDIMENTOS FINAIS

           IF  ROTSAI-COD-RETORNO NOT EQUAL '0000' AND '0002' AND
                                            '0003' AND '0004' AND
                                            '0005'
               NEXT SENTENCE
           ELSE
               MOVE  'DCOM5053'    TO  ERR-PGM
               MOVE  SPACES        TO  LKG-SQLCA
               MOVE  ERRO-AREA     TO  LKG-ERRO-AREA
           END-IF.

           GOBACK.

      *---------------------------------------------------------------
       010-999-FIM.
      *---------------------------------------------------------------

      *---------------------------------------------------------------
       005-005-OBTEM-TIMESTAMP              SECTION.
      *---------------------------------------------------------------

           MOVE  'DCOM7999'        TO     WRK-MODULO.

           CALL      WRK-MODULO    USING
                                   WRK-TIMESTAMP-ATUAL
                                   LKG-ERRO-AREA
                                   LKG-SQLCA.

      *---------------------------------------------------------------
       005-005-FIM.
      *---------------------------------------------------------------

      *---------------------------------------------------------------
       005-010-VALIDA-PESSOA                SECTION.
      *---------------------------------------------------------------

      * VALIDA PESSOA CNPJ/CPF+TP.ISENC+ABRANGENCIA

           MOVE  'PARCIAL-02'                TO 48-INSTRUCAO.
4S2511*    MOVE  ROTENT-CCNPJ-CPF            TO 48-CCNPJ-CPF.
4S2511*    MOVE  ROTENT-CFLIAL-CNPJ          TO 48-CFLIAL-CNPJ.
4S2511     MOVE  ROTENT-CCNPJ-CPF            TO 48-CCNPJ-CPF-ST.
4S2511     MOVE  ROTENT-CFLIAL-CNPJ          TO 48-CFLIAL-CNPJ-ST.
           MOVE  1                           TO 48-CABRGC-ISENC-DESC.
           MOVE  ROTENT-DINIC-VGCIA-ISENC    TO 48-DINIC-VGCIA-ISENC.
           MOVE  ROTENT-DFIM-VGCIA-ISENC     TO 48-DFIM-VGCIA-ISENC.
           MOVE  ROTENT-CTPO-ISENC-DESC      TO 48-CTPO-ISENC-DESC.
           MOVE  'DCOM6740'                  TO WRK-MODULO.

           CALL  WRK-MODULO                    USING
                                               48-ARGUMENTOS-ENTRADA
                                               48-RETORNO
                                               LKG-ERRO-AREA
                                               LKG-SQLCA.

           IF 48-COD-RETORNO EQUAL ZEROS  OR 3
              IF 48-COD-RETORNO EQUAL ZEROS
                 MOVE 'S'                     TO WRK-TEM-PESSOA
                 MOVE 48RT-NSEQ-PSSOA-DESC(1) TO WRK-NULT-SEQ-ELMTO-S
                                                 WRK-SEQUENCIA
              END-IF
           ELSE
              MOVE   '0099'         TO  ROTSAI-COD-RETORNO
              MOVE   SG-MENSAGEM(852)
                                    TO  ROTSAI-MSG-RETORNO
              PERFORM  010-010-FINALIZAR
           END-IF.

      * VALIDA SUBPRODUTO

           MOVE ROTENT-DINIC-VGCIA-ISENC       (1:2) TO
                WRK-DINIC-VGCIA-ISENC-ENT      (9:2)
           MOVE '.'                                  TO
                WRK-DINIC-VGCIA-ISENC-ENT      (8:1)
           MOVE ROTENT-DINIC-VGCIA-ISENC       (4:2) TO
                WRK-DINIC-VGCIA-ISENC-ENT      (6:2)
           MOVE '.'                                  TO
                WRK-DINIC-VGCIA-ISENC-ENT      (5:1)
           MOVE ROTENT-DINIC-VGCIA-ISENC       (7:4) TO
                WRK-DINIC-VGCIA-ISENC-ENT      (1:4)

           MOVE ROTENT-DFIM-VGCIA-ISENC        (1:2) TO
                WRK-DFIM-VGCIA-ISENC-ENT       (9:2)
           MOVE '.'                                  TO
                WRK-DFIM-VGCIA-ISENC-ENT       (8:1)
           MOVE ROTENT-DFIM-VGCIA-ISENC        (4:2) TO
                WRK-DFIM-VGCIA-ISENC-ENT       (6:2)
           MOVE '.'                                  TO
                WRK-DFIM-VGCIA-ISENC-ENT       (5:1)
           MOVE ROTENT-DFIM-VGCIA-ISENC        (7:4) TO
                WRK-DFIM-VGCIA-ISENC-ENT       (1:4)

           MOVE 'N' TO WRK-ENTRE-VIGENCIAS.

           MOVE  'PARCIAL-41'                TO 63-INSTRUCAO.
4S2511*     MOVE  ROTENT-CCNPJ-CPF            TO 63-CCNPJ-CPF.
4S2511*     MOVE  ROTENT-CFLIAL-CNPJ          TO 63-CFLIAL-CNPJ.
4S2511     MOVE  ROTENT-CCNPJ-CPF            TO 63-CCNPJ-CPF-ST.
4S2511     MOVE  ROTENT-CFLIAL-CNPJ          TO 63-CFLIAL-CNPJ-ST.
           MOVE  ROTENT-CTPO-ISENC-DESC      TO 63-CTPO-ISENC-DESC.
           MOVE  ROTENT-CPRODT               TO 63-CPRODT.
           MOVE  ROTENT-CSPROD-DESC-COML     TO 63-CSPROD-DESC-COML.
           MOVE  ZEROS                       TO 63-QTDE-A-DESPREZAR.
           MOVE  'DCOM6816'                  TO WRK-MODULO.

           CALL  WRK-MODULO                    USING
                                               63-ARGUMENTOS-ENTRADA
                                               63-RETORNO
                                               LKG-ERRO-AREA
                                               LKG-SQLCA.

           PERFORM UNTIL (63-COD-RETORNO  NOT EQUAL ZEROS AND 2) OR
                         (WRK-ENTRE-VIGENCIAS EQUAL 'S')

               PERFORM VARYING IND-LKG FROM 1 BY 1
                       UNTIL (IND-LKG GREATER 63-QTDE-RETORNADA) OR
                             (WRK-ENTRE-VIGENCIAS EQUAL 'S')

                   MOVE 63RT-DINIC-VGCIA-ISENC(IND-LKG)(1:2) TO
                        WRK-DINIC-VGCIA-ISENC-TAB      (9:2)
                   MOVE '.'                                  TO
                        WRK-DINIC-VGCIA-ISENC-TAB      (8:1)
                   MOVE 63RT-DINIC-VGCIA-ISENC(IND-LKG)(4:2) TO
                        WRK-DINIC-VGCIA-ISENC-TAB      (6:2)
                   MOVE '.'                                  TO
                        WRK-DINIC-VGCIA-ISENC-TAB      (5:1)
                   MOVE 63RT-DINIC-VGCIA-ISENC(IND-LKG)(7:4) TO
                        WRK-DINIC-VGCIA-ISENC-TAB      (1:4)

                   MOVE 63RT-DFIM-VGCIA-ISENC(IND-LKG)(1:2) TO
                        WRK-DFIM-VGCIA-ISENC-TAB      (9:2)
                   MOVE '.'                                  TO
                        WRK-DFIM-VGCIA-ISENC-TAB      (8:1)
                   MOVE 63RT-DFIM-VGCIA-ISENC(IND-LKG)(4:2) TO
                        WRK-DFIM-VGCIA-ISENC-TAB      (6:2)
                   MOVE '.'                                  TO
                        WRK-DFIM-VGCIA-ISENC-TAB      (5:1)
                   MOVE 63RT-DFIM-VGCIA-ISENC(IND-LKG)(7:4) TO
                        WRK-DFIM-VGCIA-ISENC-TAB      (1:4)

                   IF (WRK-DINIC-VGCIA-ISENC-ENT  NOT GREATER
                       WRK-DINIC-VGCIA-ISENC-TAB  AND
                       WRK-DFIM-VGCIA-ISENC-ENT   NOT LESS
                       WRK-DINIC-VGCIA-ISENC-TAB) OR
                      (WRK-DINIC-VGCIA-ISENC-ENT  NOT GREATER
                       WRK-DFIM-VGCIA-ISENC-TAB   AND
                       WRK-DFIM-VGCIA-ISENC-ENT   NOT LESS
                       WRK-DFIM-VGCIA-ISENC-TAB)  OR
                      (WRK-DINIC-VGCIA-ISENC-ENT  NOT LESS
                       WRK-DINIC-VGCIA-ISENC-TAB  AND
                       WRK-DFIM-VGCIA-ISENC-ENT   NOT GREATER
                       WRK-DFIM-VGCIA-ISENC-TAB)
                       MOVE 'S' TO WRK-ENTRE-VIGENCIAS
                   END-IF

               END-PERFORM

               IF  63-COD-RETORNO      EQUAL 2 AND
                   WRK-ENTRE-VIGENCIAS EQUAL 'N'
4S2511*             MOVE ROTENT-CCNPJ-CPF        TO
4S2511*                                          63-CCNPJ-CPF
4S2511*             MOVE  ROTENT-CFLIAL-CNPJ     TO
4S2511*                                          63-CFLIAL-CNPJ
4S2511             MOVE ROTENT-CCNPJ-CPF        TO
4S2511                                           63-CCNPJ-CPF-ST
4S2511             MOVE  ROTENT-CFLIAL-CNPJ     TO
4S2511                                           63-CFLIAL-CNPJ-ST
                   MOVE ROTENT-CTPO-ISENC-DESC  TO
                                                63-CTPO-ISENC-DESC
                   MOVE ROTENT-CPRODT           TO
                                                63-CPRODT
                   MOVE ROTENT-CSPROD-DESC-COML TO
                                                63-CSPROD-DESC-COML
                   MOVE 63-PROXIMO-RESTART      TO
                                                63-QTDE-A-DESPREZAR
                   MOVE 'PARCIAL-41'            TO
                                                63-INSTRUCAO
                   MOVE 'DCOM6816'              TO
                                                WRK-MODULO
                   CALL  WRK-MODULO USING 63-ARGUMENTOS-ENTRADA
                                          63-RETORNO
                                          LKG-ERRO-AREA
                                          LKG-SQLCA
               ELSE
                   MOVE 3 TO 63-COD-RETORNO
               END-IF

           END-PERFORM.

           IF  63-COD-RETORNO NOT EQUAL ZEROS AND 2 AND 3
               MOVE '0099'         TO  ROTSAI-COD-RETORNO
               MOVE SG-MENSAGEM(853)
                                   TO  ROTSAI-MSG-RETORNO
               PERFORM  010-010-FINALIZAR
           END-IF.

           IF  WRK-ENTRE-VIGENCIAS EQUAL 'S'
               MOVE '0854'         TO ROTSAI-COD-RETORNO
               MOVE SG-MENSAGEM(854)
                                   TO  ROTSAI-MSG-RETORNO
               PERFORM  010-010-FINALIZAR
           END-IF.

      *---------------------------------------------------------------
       005-010-FIM.
      *---------------------------------------------------------------

      *---------------------------------------------------------------
       010-000-TRATAR-NEGOCIO               SECTION.
      *---------------------------------------------------------------

           IF    WRK-TEM-PESSOA NOT EQUAL 'S'
                 PERFORM 010-010-TRATAR-PESSOA
           END-IF.

           PERFORM 010-020-TRATAR-SUBPRODUTO.

      *---------------------------------------------------------------
       010-000-FIM.
      *---------------------------------------------------------------

      *---------------------------------------------------------------
       010-010-TRATAR-PESSOA                SECTION.
      *---------------------------------------------------------------

           MOVE  ROTENT-COD-RETORNO    TO  WRK-COD-RETORNO-E
           MOVE  ROTENT-MENSAGEM       TO  WRK-MSG-RETORNO-E
           MOVE  ROTENT-RESTART        TO  WRK-RESTART-E
           MOVE  ROTENT-FLAG           TO  WRK-FLAG-E
           MOVE  ROTENT-CFUNC-BDSCO    TO  WRK-CFUNC-BDSCO-E
           MOVE  ROTENT-CTERM          TO  WRK-CTERM-E

           EVALUATE ROTENT-CTPO-ISENC-DESC
             WHEN   1
                MOVE  13       TO  WRK-CELMTO-DESC-COML-E
             WHEN   2
                MOVE  14      TO   WRK-CELMTO-DESC-COML-E
           END-EVALUATE.

           MOVE  001                   TO  WRK-WCHAVE-PRINC-ELMTO-E.
           MOVE  003                   TO  WRK-QDIG-SEQ-ELMTO-E.
           MOVE  'DCOM5022'            TO  WRK-MODULO.

           CALL  WRK-MODULO            USING  WRK-ENTRADA
                                              WRK-SAIDA
                                              LKG-ERRO-AREA
                                              LKG-SQLCA.

           IF  WRK-COD-RETORNO-S      NOT  EQUAL  ZEROS
               MOVE  '0050'           TO  ROTSAI-COD-RETORNO
               MOVE  SG-MENSAGEM(855)
                                      TO  ROTSAI-MSG-RETORNO
               PERFORM  010-010-FINALIZAR
           END-IF.

           MOVE WRK-NULT-SEQ-ELMTO-S TO WRK-SEQUENCIA.

      * INCLUIR REGISTRO TABELA PESSOA-ISENCAO

4S2511*    MOVE   ROTENT-CCNPJ-CPF          TO 48-CCNPJ-CPF
4S2511     MOVE   ROTENT-CCNPJ-CPF          TO 48-CCNPJ-CPF-ST
           MOVE   WRK-SEQUENCIA             TO 48-NSEQ-PSSOA-DESC
           MOVE   ROTENT-DINIC-VGCIA-ISENC  TO 48-DINIC-VGCIA-ISENC
           MOVE   ROTENT-CTPO-ISENC-DESC    TO 48-CTPO-ISENC-DESC
           MOVE   ROTENT-DFIM-VGCIA-ISENC   TO 48-DFIM-VGCIA-ISENC
4S2511*    XMOVE   ROTENT-CFLIAL-CNPJ        TO 48-CFLIAL-CNPJ
4S2511*    XMOVE   ROTENT-CCTRL-CNPJ-CPF     TO 48-CCTRL-CNPJ-CPF
4S2511     MOVE   ROTENT-CFLIAL-CNPJ        TO 48-CFLIAL-CNPJ-ST
4S2511     MOVE   ROTENT-CCTRL-CNPJ-CPF     TO 48-CCTRL-CNPJ-CPF-ST
           MOVE   ZEROS                     TO 48-CCLUB
           MOVE   1                         TO 48-CABRGC-ISENC-DESC

4S2511     IF     ROTENT-CFLIAL-CNPJ      EQUAL   SPACES OR
4S2511            ROTENT-CFLIAL-CNPJ      EQUAL   LOW-VALUES
                  MOVE 'FI'       TO      48-CINDCD-PSSOA-DESC
           ELSE
                  MOVE 'JU'       TO      48-CINDCD-PSSOA-DESC
           END-IF.

           MOVE   'N'                      TO  48-CINDCD-REST-CDENT
           MOVE   ROTENT-CFUNC-BDSCO       TO  48-CFUNC-BDSCO
           MOVE   ROTENT-CTERM             TO  48-CTERM
           MOVE  'DCOM6742'                TO  WRK-MODULO

           CALL   WRK-MODULO      USING        48-ARGUMENTOS-ENTRADA
                                               48-RETORNO
                                               LKG-ERRO-AREA
                                               LKG-SQLCA.

           IF  48-COD-RETORNO NOT EQUAL ZEROS
               IF  48-COD-RETORNO EQUAL 4
                   MOVE '0856'         TO  ROTSAI-COD-RETORNO
                   MOVE SG-MENSAGEM(856)
                                       TO  ROTSAI-MSG-RETORNO
               ELSE
                   MOVE '0099'         TO  ROTSAI-COD-RETORNO
                   MOVE SG-MENSAGEM(857)
                                       TO  ROTSAI-MSG-RETORNO
               END-IF
               PERFORM  010-010-FINALIZAR
           END-IF.

      *---------------------------------------------------------------
       010-010-FIM.
      *---------------------------------------------------------------

      *---------------------------------------------------------------
       010-020-TRATAR-SUBPRODUTO            SECTION.
      *---------------------------------------------------------------
      * INCLUIR TABELA SUBPRODUTO-PESSOA-ISENCAO

4S2511*    MOVE ROTENT-CCNPJ-CPF          TO 63-CCNPJ-CPF
4S2511     MOVE ROTENT-CCNPJ-CPF          TO 63-CCNPJ-CPF-ST
           MOVE WRK-SEQUENCIA             TO 63-NSEQ-PSSOA-DESC
           MOVE ROTENT-DINIC-VGCIA-ISENC  TO 63-DINIC-VGCIA-ISENC
           MOVE ROTENT-CTPO-ISENC-DESC    TO 63-CTPO-ISENC-DESC
           MOVE ROTENT-DFIM-VGCIA-ISENC   TO 63-DFIM-VGCIA-ISENC
4S2511*    MOVE ROTENT-CFLIAL-CNPJ        TO 63-CFLIAL-CNPJ
4S2511*    MOVE ROTENT-CCTRL-CNPJ-CPF     TO 63-CCTRL-CNPJ-CPF
4S2511     MOVE ROTENT-CFLIAL-CNPJ        TO 63-CFLIAL-CNPJ-ST
4S2511     MOVE ROTENT-CCTRL-CNPJ-CPF     TO 63-CCTRL-CNPJ-CPF-ST
           MOVE ZEROS                     TO 63-CCLUB
           MOVE ROTENT-CPRODT             TO 63-CPRODT
           MOVE ROTENT-CSPROD-DESC-COML   TO 63-CSPROD-DESC-COML

4S2511     IF     ROTENT-CFLIAL-CNPJ      EQUAL  SPACES OR
4S2511            ROTENT-CFLIAL-CNPJ      EQUAL  LOW-VALUES
                  MOVE 'FI'        TO      63-CINDCD-PSSOA-DESC
           ELSE
                  MOVE 'JU'        TO      63-CINDCD-PSSOA-DESC
           END-IF.

           IF     ROTENT-VREST-MAX-OPER    EQUAL  ZEROS
                  MOVE 'N'         TO      63-CINDCD-REST-VLR
                  MOVE ZEROS       TO      ROTENT-VREST-MAX-OPER
           ELSE
                  MOVE 'S'         TO      63-CINDCD-REST-VLR
           END-IF.

           MOVE ROTENT-VREST-MAX-OPER  TO 63-VREST-MAX-OPER
           MOVE ROTENT-CFUNC-BDSCO     TO 63-CFUNC-BDSCO
           MOVE ROTENT-CTERM           TO 63-CTERM
           MOVE 'DCOM6817'             TO WRK-MODULO

           CALL      WRK-MODULO         USING
                                               63-ARGUMENTOS-ENTRADA
                                               63-RETORNO
                                               LKG-ERRO-AREA
                                               LKG-SQLCA.

           IF  63-COD-RETORNO NOT EQUAL ZEROS
               IF  63-COD-RETORNO EQUAL 4
                   MOVE '0858'         TO  ROTSAI-COD-RETORNO
                   MOVE SG-MENSAGEM(858)
                                       TO  ROTSAI-MSG-RETORNO
               ELSE
                   MOVE '0099'         TO  ROTSAI-COD-RETORNO
                   MOVE SG-MENSAGEM(859)
                                 TO  ROTSAI-MSG-RETORNO
               END-IF
               PERFORM  010-010-FINALIZAR
           END-IF.

      *---------------------------------------------------------------
       010-020-FIM.
      *---------------------------------------------------------------

      *---------------------------------------------------------------
       010-005-GRAVAR-LOG                  SECTION.
      *---------------------------------------------------------------
      * GRAVAR  LOG TABELA PESSOA-ISENCAO

           MOVE 004                      TO X1-CELMTO-DESC-COML.

           IF ROTENT-CTPO-ISENC-DESC   EQUAL 1
              MOVE  058                  TO X1-CEVNTO-LOG-DESC
           ELSE
              MOVE  061                  TO X1-CEVNTO-LOG-DESC.

           INITIALIZE WRK-CCHAVE-ELMTO-DESC.

           IF  WRK-TRATA-ALTERACAO       EQUAL 'S'
               MOVE 'A'                  TO X1-CINDCD-MANUT-REG
               MOVE WRK-DATA-MENOR       TO WRK-CCHAVE-ELMTO-DESC-5
           ELSE
               MOVE ROTENT-DINIC-VGCIA-ISENC
                                         TO WRK-CCHAVE-ELMTO-DESC-5
               MOVE 'I'                  TO X1-CINDCD-MANUT-REG
           END-IF.

           MOVE ROTENT-CPRODT            TO WRK-CCHAVE-ELMTO-DESC-1.
           MOVE ROTENT-CSPROD-DESC-COML  TO WRK-CCHAVE-ELMTO-DESC-2.
           MOVE ROTENT-CCNPJ-CPF         TO WRK-CCHAVE-ELMTO-DESC-3.
           MOVE ROTENT-CTPO-ISENC-DESC   TO WRK-CCHAVE-ELMTO-DESC-6.

           MOVE WRK-CCHAVE-ELMTO-DESC    TO X1-CCHAVE-ELMTO-DESC.

           PERFORM 001-020-OBTER-DATA-VIGENTE.

           MOVE 'DCOM5053'               TO X1-CTRANS-PROG-ATULZ
           MOVE 'O'                      TO X1-CINDCD-MODLD-PROCM
           MOVE 237                      TO X1-CEMPR-INC
           MOVE 4130                     TO X1-CDEPDC
           MOVE ROTENT-CFUNC-BDSCO       TO X1-CFUNC-BDSCO
           MOVE ROTENT-CTERM             TO X1-CTERM
           MOVE 'DCOM5075'               TO WRK-MODULO

           CALL      WRK-MODULO       USING X1-REGISTRO
                                            LKG-ERRO-AREA
                                            LKG-SQLCA.

           IF  X1-COD-RETORNO NOT EQUAL ZEROS
               MOVE '0099'         TO  ROTSAI-COD-RETORNO
               MOVE SG-MENSAGEM(578)
                                 TO  ROTSAI-MSG-RETORNO
               PERFORM  010-010-FINALIZAR
           END-IF.

      *---------------------------------------------------------------
       010-005-FIM.
      *---------------------------------------------------------------
