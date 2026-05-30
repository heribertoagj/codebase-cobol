      *----------------------------------------------------------------*
       IDENTIFICATION                  DIVISION.
      *----------------------------------------------------------------*

       PROGRAM-ID.                     DCOM5060.
       AUTHOR.                         VITAL.

      *----------------------------------------------------------------*
      *                       ALTRAN CONSULTORIA                       *
      *----------------------------------------------------------------*
      *                                                                *
      *     PROGRAMA     : DCOM5060                                    *
      *     PROGRAMADOR  : VITAL                     -   ALTRAN        *
      *     ANALISTA     : MAURICIO FRANCO DA SILVA  -   ALTRAN        *
      *     DATA         : MAIO/2006                                   *
      *                                                                *
      *     OBJETIVO     : ALTERAR ISENCAO DA ANALISE DE RESTRICAO DO  *
      *                    SACADO POR CEDENTE NO GERAL.                *
      *                                                                *
      *     INC'S        :                                             *
      *       I#DCOMSG   - AREA DE RETORNO DAS MENSAGENS DE ERRO       *
      *       I#DCOM48   - TABELA TPSSOA_ISENC_DESC                    *
      *       I#DCOM56   - TABELA TSACDO_ISENC_CDENT                   *
      *       I#DCOMX1   - ATUALIZA LOG                                *
      *       I#BRAD7C   - AREA PARA TRATAMENTO DE ERROS DB2           *
      *                                                                *
      *     MODULOS      :                                             *
      *       DCOM5022   - ACESSO AOS DADOS DA TABELA DCOMB020 (SELECT)*
      *       DCOM6743   - ATUALIZACAO DA TABELA DCOMB048 (UPDATE)     *
      *       DCOM6783   - ATUALIZACAO DA TABELA DCOMB056 (UPDATE)     *
      *       DCOM5075   - ATUALIZAR LOG DA INCLUSAO REALIZADA         *
      *       POOL7100   - TRATAMENTO DE ERROS                         *
      *                                                                *
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       ENVIRONMENT                     DIVISION.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       CONFIGURATION                   SECTION.
      *----------------------------------------------------------------*

       SPECIAL-NAMES.
           DECIMAL-POINT IS COMMA.

      *----------------------------------------------------------------*
       DATA                            DIVISION.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       WORKING-STORAGE                 SECTION.
      *----------------------------------------------------------------*

       77  FILLER                      PIC  X(039)        VALUE
           '*** INICIO WORKING-STORAGE DCOM5060 ***'.

      *----------------------------------------------------------------*
       77  FILLER                      PIC  X(020)        VALUE
           '* AREAS AUXILIARES *'.
      *----------------------------------------------------------------*

       77  WRK-MODULO                  PIC  X(008)        VALUE SPACES.
       77  WRK-NSEQ-PSSOA-DESC         PIC  9(003)        VALUE ZEROS.
PAT    77  WRK-NSEQ-PSSOA-DESC-ANT     PIC  9(003)        VALUE ZEROS.
       77  WRK-ERRO                    PIC  9(003)        VALUE ZEROS.
       77  WRK-QTDE-TOTAL              PIC  9(009)        VALUE ZEROS.
       77  WRK-DINIC-VGCIA-ISENC       PIC  X(010).
       77  WRK-TIMESTAMP-ATUAL         PIC  X(026)        VALUE SPACES.
PAT    77  WRK-CINDCD-MANUT-REG        PIC  X(001).
       77  WRK-CINDCD-REST-CDENT-ANT   PIC  X(001).
       77  WRK-DATA-ANT                PIC  9(008)  VALUE  ZEROS.
       77  WRK-DATA-ATU                PIC  9(008)  VALUE  ZEROS.
       77  WRK-DATA-ATU-1              PIC  9(008)  VALUE  ZEROS.
       77  WRK-DATA-INI-ANT            PIC  9(008)  VALUE  ZEROS.
       77  WRK-DATA-INI                PIC  9(008)  VALUE  ZEROS.
       77  WRK-DATA-FIM-ANT            PIC  9(008)  VALUE  ZEROS.
       77  WRK-DATA-FIM                PIC  9(008)  VALUE  ZEROS.
       77  WRK-48-OCORRENCIAS          PIC  X(084)  VALUE  SPACES.
       77  WRK-DT-ENT                  PIC  9(9)    COMP-3 VALUE 0.
       77  WRK-NUM-DIAS                PIC S9(5)    COMP-3 VALUE +1.
       77  WRK-DT-SAI                  PIC  9(9)    COMP-3 VALUE 0.
       77  WRK-DATA-CHAVE              PIC  X(10)   VALUE SPACES.

       01  WRK-ENTRADA.
           03  WRK-COD-RETORNO-E       PIC  X(004).
           03  WRK-MSG-RETORNO-E       PIC  X(079).
           03  WRK-RESTART-E           PIC  9(005).
           03  WRK-FLAG-E              PIC  X(001).
           03  WRK-CFUNC-BDSCO-E       PIC  9(009).
           03  WRK-CTERM-E             PIC  X(008).
           03  WRK-DADOS-E.
               05  WRK-CELMTO-DESC-COML-E     PIC  9(03).
               05  WRK-WCHAVE-PRINC-ELMTO-E   PIC  9(09).
               05  WRK-QDIG-SEQ-ELMTO-E       PIC  9(03).

       01  WRK-SAIDA.
           03  WRK-HEADER-S.
               05  WRK-COD-RETORNO-S          PIC  X(04).
               05  WRK-MSG-RETORNO-S          PIC  X(79).
               05  WRK-RESTART-S              PIC  9(05).
               05  WRK-FLAG-S                 PIC  X(01).
               05  WRK-NULT-SEQ-ELMTO-S       PIC  9(15).

       01  WRK-MSG.
           03 WRK-DINIC                 PIC X(10).
           03 WRK-CTPO-ISENC-DESC       PIC  9(003).
4S2511*    03 WRK-CCNPJ-CPF             PIC  9(009).
4S2511     03 WRK-CCNPJ-CPF             PIC  X(009).
4S2511*    03 WRK-CCNPJ-CPF-PROPN       PIC  9(009).
4S2511     03 WRK-CCNPJ-CPF-PROPN       PIC  X(009).

       01  WRK-CCHAVE-ELMTO-DESC.
4S2511*    03  WRK-CCHAVE-ELMTO-DESC-1        PIC  9(009) VALUE ZEROS.
4S2511     03  WRK-CCHAVE-ELMTO-DESC-1        PIC  X(009) VALUE SPACES.
           03  WRK-CCHAVE-ELMTO-DESC-2        PIC  X(010) VALUE SPACES.
           03  WRK-CCHAVE-ELMTO-DESC-3        PIC  9(003) VALUE ZEROS.
           03  FILLER                         PIC  X(020) VALUE SPACES.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(027)        VALUE
           '* AREA DO MODULO DCOM5022 *'.
      *----------------------------------------------------------------*

       COPY 'I#DCOM20'.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(027)        VALUE
           '* AREA DO MODULO DCOM6743 *'.
      *----------------------------------------------------------------*

       COPY 'I#DCOM48'.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(027)        VALUE
           '* AREA DO MODULO DCOM6783 *'.
      *----------------------------------------------------------------*

       COPY 'I#DCOM56'.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(027)        VALUE
           '* AREA DO MODULO DCOM5075 *'.
      *----------------------------------------------------------------*

       COPY 'I#DCOMX1'.

      *----------------------------------------------------------------*
       77  FILLER                      PIC  X(029)        VALUE
           '* AREA DE MENSAGENS DO DCOM *'.
      *----------------------------------------------------------------*

       COPY 'I#DCOMSG'.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(020)        VALUE
           '* AREA DA POOL7100 *'.
      *----------------------------------------------------------------*

       COPY 'I#BRAD7C'.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(039)        VALUE
           '*** FINAL WORKING-STORAGE DCOM5060 ***'.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       LINKAGE                         SECTION.
      *----------------------------------------------------------------*

       01  ENTRADA-ROTEADOR.
           03  ROTENT-COD-RETORNO           PIC  X(004).
           03  ROTENT-MSG-RETORNO           PIC  X(079).
           03  ROTENT-RESTART               PIC  9(005).
           03  ROTENT-FLAG                  PIC  X(001).
           03  ROTENT-CFUNC-BDSCO           PIC  9(009).
           03  ROTENT-CTERM                 PIC  X(008).
           03  ROTENT-CTPO-ISENC-DESC       PIC  9(003).
4S2511*    03  ROTENT-CCNPJ-CPF             PIC  9(009).
4S2511     03  ROTENT-CCNPJ-CPF             PIC  X(009).
4S2511*    03  ROTENT-CFLIAL-CNPJ           PIC  9(005).
4S2511     03  ROTENT-CFLIAL-CNPJ           PIC  X(004).
           03  ROTENT-CCTRL-CNPJ-CPF        PIC  9(002).
           03  ROTENT-DINIC-VGCIA-ISENC-ANT PIC  X(010).
           03  ROTENT-DINIC-VGCIA-ISENC     PIC  X(010).
           03  ROTENT-DFIM-VGCIA-ISENC-ANT  PIC  X(010).
           03  ROTENT-DFIM-VGCIA-ISENC      PIC  X(010).
           03  ROTENT-CINDCD-REST-CDENT     PIC  X(001).
4S2511*    03  ROTENT-CCNPJ-CPF-PROPN-ANT   PIC  9(009).
4S2511     03  ROTENT-CCNPJ-CPF-PROPN-ANT   PIC  X(009).
4S2511*    03  ROTENT-CFLIAL-CNPJ-PROPN-ANT PIC  9(005).
4S2511     03  ROTENT-CFLIAL-CNPJ-PROPN-ANT PIC  X(004).
           03  ROTENT-CCTRL-CNPJ-PROPN-ANT  PIC  9(002).
4S2511*    03  ROTENT-CCNPJ-CPF-PROPN       PIC  9(009).
4S2511     03  ROTENT-CCNPJ-CPF-PROPN       PIC  X(009).
4S2511*    03  ROTENT-CFLIAL-CNPJ-PROPN     PIC  9(005).
4S2511     03  ROTENT-CFLIAL-CNPJ-PROPN     PIC  X(004).
           03  ROTENT-CCTRL-CNPJ-PROPN      PIC  9(002).

       01  SAIDA-ROTEADOR.
           03  ROTSAI-HEADER.
               05  ROTSAI-COD-RETORNO  PIC  X(004).
               05  ROTSAI-MSG-RETORNO  PIC  X(079).
               05  ROTSAI-RESTART      PIC  9(005).
               05  ROTSAI-FLAG         PIC  X(001).
           03  ROTSAI-CONSISTENCIA.
               05  ROTSAI-TAMANHO-ERRO PIC  9(002).
               05  ROTSAI-TABELA-ERROS OCCURS  014  TIMES.
                   07  ROTSAI-CAMPOS-ERRO
                                       PIC  9(001).

       01  LKG-ERRO-AREA               PIC  X(107).
       01  LKG-SQLCA                   PIC  X(136).

      *----------------------------------------------------------------*
       PROCEDURE DIVISION           USING  ENTRADA-ROTEADOR
                                           SAIDA-ROTEADOR
                                           LKG-ERRO-AREA
                                           LKG-SQLCA.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       00000-PRINCIPAL                 SECTION.
      *----------------------------------------------------------------*

           PERFORM  10000-INICIALIZAR.

           PERFORM  20000-CONSISTIR-ENTRADA.

           PERFORM  30000-PROCESSAR.

           PERFORM  70000-FINALIZAR.

      *----------------------------------------------------------------*
       00000-99-FIM.                   EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       10000-INICIALIZAR               SECTION.
      *----------------------------------------------------------------*

           INITIALIZE                      SAIDA-ROTEADOR
                                           20-ARGUMENTOS-ENTRADA
                                           48-ARGUMENTOS-ENTRADA
                                           56-ARGUMENTOS-ENTRADA
                                           X1-ARGUMENTOS-ENTRADA
                                           LKG-ERRO-AREA
                                           LKG-SQLCA
                                           ERRO-AREA.

           MOVE  2                     TO  ROTSAI-TAMANHO-ERRO.
           MOVE  '0000'                TO  ROTSAI-COD-RETORNO.
           MOVE  SG-MENSAGEM(001)      TO  ROTSAI-MSG-RETORNO.

      *----------------------------------------------------------------*
       10000-99-FIM.                   EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       20000-CONSISTIR-ENTRADA         SECTION.
      *----------------------------------------------------------------*

           IF  ROTENT-CTPO-ISENC-DESC  NOT  NUMERIC OR
               ROTENT-CTPO-ISENC-DESC  NOT  EQUAL  1
               MOVE  1                 TO ROTSAI-CAMPOS-ERRO(1)
               MOVE '0495'             TO ROTSAI-COD-RETORNO
               MOVE SG-MENSAGEM(0495)  TO ROTSAI-MSG-RETORNO
               PERFORM 70000-FINALIZAR
           END-IF.

4S2511*    IF  ROTENT-CCNPJ-CPF        NOT NUMERIC OR
4S2511*        ROTENT-CCNPJ-CPF        EQUAL  ZEROS
4S2511     IF  ROTENT-CCNPJ-CPF        EQUAL  SPACES OR
4S2511                                 LOW-VALUES
               MOVE  1                 TO ROTSAI-CAMPOS-ERRO(2)
               MOVE '0223'             TO ROTSAI-COD-RETORNO
               MOVE SG-MENSAGEM(0223)  TO ROTSAI-MSG-RETORNO
               PERFORM 70000-FINALIZAR
           END-IF.

           IF  ROTENT-DINIC-VGCIA-ISENC    EQUAL SPACES
               MOVE  1                 TO ROTSAI-CAMPOS-ERRO(3)
               MOVE '0540'             TO ROTSAI-COD-RETORNO
               MOVE SG-MENSAGEM(0540)  TO ROTSAI-MSG-RETORNO
               PERFORM 70000-FINALIZAR
           END-IF.

           IF  ROTENT-DFIM-VGCIA-ISENC     EQUAL SPACES
               MOVE  1                 TO ROTSAI-CAMPOS-ERRO(4)
               MOVE '0544'             TO ROTSAI-COD-RETORNO
               MOVE SG-MENSAGEM(0544)  TO ROTSAI-MSG-RETORNO
               PERFORM 70000-FINALIZAR
           END-IF.

           IF  ROTENT-CINDCD-REST-CDENT
                                       NOT EQUAL  'S'  AND  'N'
               MOVE  1                 TO ROTSAI-CAMPOS-ERRO(5)
               MOVE '0764'             TO ROTSAI-COD-RETORNO
               MOVE SG-MENSAGEM(0764)  TO ROTSAI-MSG-RETORNO
               PERFORM 70000-FINALIZAR
           END-IF.

           IF  ROTENT-CINDCD-REST-CDENT
                                       EQUAL  'S'
4S2511*    AND ROTENT-CCNPJ-CPF-PROPN  EQUAL  ZEROS
4S2511     AND ROTENT-CCNPJ-CPF-PROPN  EQUAL  SPACES OR
4S2511                                  LOW-VALUES
               MOVE  1                 TO ROTSAI-CAMPOS-ERRO(6)
               MOVE '0549'             TO ROTSAI-COD-RETORNO
               MOVE SG-MENSAGEM(0549)  TO ROTSAI-MSG-RETORNO
               PERFORM 70000-FINALIZAR
           END-IF.

           IF  ROTENT-CINDCD-REST-CDENT
                                       EQUAL  'N'
4S2511*    AND ROTENT-CCNPJ-CPF-PROPN  NOT EQUAL  ZEROS
4S2511     AND ROTENT-CCNPJ-CPF-PROPN  NOT EQUAL  SPACES AND
4S2511                                  NOT EQUAL  LOW-VALUES
               MOVE  1                 TO ROTSAI-CAMPOS-ERRO(6)
               MOVE '0765'             TO ROTSAI-COD-RETORNO
               MOVE SG-MENSAGEM(0765)  TO ROTSAI-MSG-RETORNO
               PERFORM 70000-FINALIZAR
           END-IF.

      *----------------------------------------------------------------*
       20000-99-FIM.                   EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       30000-PROCESSAR                 SECTION.
      *----------------------------------------------------------------*

           PERFORM  31000-OBTEM-TIMESTAMP.

           PERFORM  40000-TRATAR-NEGOCIO.

      *----------------------------------------------------------------*
       30000-99-FIM.                   EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       31000-OBTEM-TIMESTAMP           SECTION.
      *----------------------------------------------------------------*

           MOVE  'DCOM7999'         TO  WRK-MODULO.

           CALL  WRK-MODULO         USING  WRK-TIMESTAMP-ATUAL
                                           LKG-ERRO-AREA
                                           LKG-SQLCA.

      *----------------------------------------------------------------*
       31000-99-FIM.                   EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       40000-TRATAR-NEGOCIO            SECTION.
      *----------------------------------------------------------------*

           PERFORM  41010-OBTER-DATA-LOG.

           PERFORM  41000-INVERTER-DATA.

           IF (WRK-DATA-INI-ANT   NOT =  WRK-DATA-INI AND
               WRK-DATA-INI   <=  WRK-DATA-ATU)
               MOVE  '0145'            TO  ROTSAI-COD-RETORNO
               MOVE  SG-MENSAGEM(0145)
                                       TO  ROTSAI-MSG-RETORNO
               PERFORM  70000-FINALIZAR
           END-IF.

           IF  WRK-DATA-FIM   <   WRK-DATA-ATU
               MOVE  '0247'            TO  ROTSAI-COD-RETORNO
               MOVE  SG-MENSAGEM(0247)
                                       TO  ROTSAI-MSG-RETORNO
               PERFORM  70000-FINALIZAR
           END-IF.

4S2511*    IF  ROTENT-CCNPJ-CPF-PROPN  EQUAL  ZEROS
4S2511     IF  ROTENT-CCNPJ-CPF-PROPN  EQUAL  SPACES OR
4S2511                                  LOW-VALUES
               PERFORM  42000-VALIDAR-PESSOA
           ELSE
               PERFORM  42500-VALIDAR-PROPONENTE
           END-IF.

           PERFORM  42750-VALIDA-ELMTO-COUNT.

           IF  WRK-DATA-INI   <=  WRK-DATA-ATU
               IF  WRK-DATA-FIM-ANT  =  WRK-DATA-ATU
                   IF  WRK-DATA-FIM-ANT  =  WRK-DATA-FIM
                       MOVE '0766'      TO   ROTSAI-COD-RETORNO
                       MOVE SG-MENSAGEM(0766)
                                       TO   ROTSAI-MSG-RETORNO
                      PERFORM  70000-FINALIZAR
                   ELSE
                      PERFORM  43000-ENCERRAR-ISENCOES-ANT

                      MOVE 'A'    TO    WRK-CINDCD-MANUT-REG
                      PERFORM  60000-GRAVAR-LOG

                      PERFORM  43500-SOMA-DIA
                      PERFORM  45000-INCLUIR-ISENCOES
                   END-IF
               ELSE
                  PERFORM  43000-ENCERRAR-ISENCOES-ANT
                  IF  WRK-DATA-FIM  NOT =  WRK-DATA-ATU
                      MOVE 'A'    TO    WRK-CINDCD-MANUT-REG
PAT                   PERFORM  60000-GRAVAR-LOG
                      PERFORM  43500-SOMA-DIA
                      PERFORM  45000-INCLUIR-ISENCOES
                  ELSE
                      MOVE '9999' TO   ROTSAI-COD-RETORNO
                      MOVE SG-MENSAGEM(0767)
                                  TO   ROTSAI-MSG-RETORNO
                      PERFORM  70000-FINALIZAR
                  END-IF
           ELSE
               PERFORM  44000-EXCLUIR-ISENCOES-ANT
               MOVE     ROTENT-DINIC-VGCIA-ISENC TO
                        WRK-DINIC-VGCIA-ISENC
               PERFORM  45000-INCLUIR-ISENCOES
           END-IF.

      *----------------------------------------------------------------*
       40000-99-FIM.                   EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       41010-OBTER-DATA-LOG            SECTION.
      *----------------------------------------------------------------*

4S2511*    MOVE  ROTENT-CCNPJ-CPF         TO 48-CCNPJ-CPF.
4S2511     MOVE  ROTENT-CCNPJ-CPF         TO 48-CCNPJ-CPF-ST.
           MOVE  ROTENT-CTPO-ISENC-DESC   TO 48-CTPO-ISENC-DESC.
           MOVE  'PARCIAL-20'             TO 48-INSTRUCAO.
           MOVE  'DCOM6741'               TO WRK-MODULO.

           CALL  WRK-MODULO            USING 48-ARGUMENTOS-ENTRADA
                                             48-RETORNO
                                             LKG-ERRO-AREA
                                             LKG-SQLCA
                                             ROTENT-DINIC-VGCIA-ISENC
                                             ROTENT-DFIM-VGCIA-ISENC.

           IF  48-COD-RETORNO          NOT EQUAL ZEROS AND 002 AND 003
               MOVE 48-COD-RETORNO     TO ROTSAI-COD-RETORNO
               MOVE 48-MENSAGEM        TO ROTSAI-MSG-RETORNO
               PERFORM  70000-FINALIZAR
           END-IF.

           MOVE 48RT-DINIC-VGCIA-ISENC(1)
                                       TO WRK-DATA-CHAVE.

4S2511*    MOVE  ROTENT-CCNPJ-CPF         TO 48-CCNPJ-CPF.
4S2511     MOVE  ROTENT-CCNPJ-CPF         TO 48-CCNPJ-CPF-ST.
4S2511*    MOVE  ROTENT-CFLIAL-CNPJ       TO 48-CFLIAL-CNPJ
4S2511     MOVE  ROTENT-CFLIAL-CNPJ       TO 48-CFLIAL-CNPJ-ST
           MOVE  ROTENT-CCTRL-CNPJ-CPF    TO 48-CCTRL-CNPJ-CPF
4S2511     MOVE  ROTENT-CCTRL-CNPJ-CPF    TO 48-CCTRL-CNPJ-CPF-ST
           MOVE  ROTENT-CTPO-ISENC-DESC   TO 48-CTPO-ISENC-DESC.
           MOVE  'PARCIAL-02'             TO 48-INSTRUCAO.
           MOVE  'DCOM6741'               TO WRK-MODULO.

           CALL  WRK-MODULO            USING 48-ARGUMENTOS-ENTRADA
                                             48-RETORNO
                                             LKG-ERRO-AREA
                                             LKG-SQLCA
                                             ROTENT-DINIC-VGCIA-ISENC
                                             ROTENT-DFIM-VGCIA-ISENC.

           IF 48-COD-RETORNO           NOT EQUAL ZEROS OR 2 OR 3
              IF 48-COD-RETORNO        NOT EQUAL 0099
                 MOVE 48-COD-RETORNO   TO ROTSAI-COD-RETORNO
                 MOVE 48-MENSAGEM      TO ROTSAI-MSG-RETORNO
              ELSE
                 MOVE '0675'           TO ROTSAI-COD-RETORNO
                 MOVE SG-MENSAGEM(0675)
                                       TO ROTSAI-MSG-RETORNO
              END-IF
              PERFORM 70000-FINALIZAR
           END-IF.

      *----------------------------------------------------------------*
       40010-99-FIM.                   EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       41000-INVERTER-DATA             SECTION.
      *----------------------------------------------------------------*

           MOVE  WRK-TIMESTAMP-ATUAL(1:4)
                                       TO  WRK-DATA-ATU(1:4).
           MOVE  WRK-TIMESTAMP-ATUAL(6:2)
                                       TO  WRK-DATA-ATU(5:2).
           MOVE  WRK-TIMESTAMP-ATUAL(9:2)
                                       TO  WRK-DATA-ATU(7:2).

           MOVE  ROTENT-DINIC-VGCIA-ISENC(1:2)
                                       TO  WRK-DATA-INI(7:2).
           MOVE  ROTENT-DINIC-VGCIA-ISENC(4:2)
                                       TO  WRK-DATA-INI(5:2).
           MOVE  ROTENT-DINIC-VGCIA-ISENC(7:4)
                                       TO  WRK-DATA-INI(1:4).

           MOVE  ROTENT-DFIM-VGCIA-ISENC(1:2)
                                       TO  WRK-DATA-FIM(7:2).
           MOVE  ROTENT-DFIM-VGCIA-ISENC(4:2)
                                       TO  WRK-DATA-FIM(5:2).
           MOVE  ROTENT-DFIM-VGCIA-ISENC(7:4)
                                       TO  WRK-DATA-FIM(1:4).

           MOVE  ROTENT-DINIC-VGCIA-ISENC-ANT(1:2)
                                       TO  WRK-DATA-INI-ANT(7:2).
           MOVE  ROTENT-DINIC-VGCIA-ISENC-ANT(4:2)
                                       TO  WRK-DATA-INI-ANT(5:2).
           MOVE  ROTENT-DINIC-VGCIA-ISENC-ANT(7:4)
                                       TO  WRK-DATA-INI-ANT(1:4).

           MOVE  ROTENT-DFIM-VGCIA-ISENC-ANT(1:2)
                                       TO  WRK-DATA-FIM-ANT(7:2).
           MOVE  ROTENT-DFIM-VGCIA-ISENC-ANT(4:2)
                                       TO  WRK-DATA-FIM-ANT(5:2).
           MOVE  ROTENT-DFIM-VGCIA-ISENC-ANT(7:4)
                                       TO  WRK-DATA-FIM-ANT(1:4).

      *----------------------------------------------------------------*
       41000-99-FIM.                   EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       42000-VALIDAR-PESSOA            SECTION.
      *----------------------------------------------------------------*

           MOVE  'PARCIAL-82'             TO 48-INSTRUCAO.
4S2511*    MOVE  ROTENT-CCNPJ-CPF         TO 48-CCNPJ-CPF.
4S2511     MOVE  ROTENT-CCNPJ-CPF         TO 48-CCNPJ-CPF-ST.
4S2511*    MOVE  ROTENT-CFLIAL-CNPJ       TO 48-CFLIAL-CNPJ.
4S2511     MOVE  ROTENT-CFLIAL-CNPJ       TO 48-CFLIAL-CNPJ-ST.
           MOVE  ROTENT-DINIC-VGCIA-ISENC TO 48-DINIC-VGCIA-ISENC.
           MOVE  ROTENT-DFIM-VGCIA-ISENC  TO 48-DFIM-VGCIA-ISENC.
           MOVE  ROTENT-CTPO-ISENC-DESC   TO 48-CTPO-ISENC-DESC.
           MOVE  005                      TO 48-CABRGC-ISENC-DESC.

           MOVE  'DCOM6741'               TO WRK-MODULO.

           CALL  WRK-MODULO            USING
                                       48-ARGUMENTOS-ENTRADA
                                       48-RETORNO
                                       LKG-ERRO-AREA
                                       LKG-SQLCA
                                       ROTENT-DINIC-VGCIA-ISENC-ANT
                                       ROTENT-DFIM-VGCIA-ISENC-ANT.

           IF 48-COD-RETORNO EQUAL ZEROS OR 2 OR 3
              MOVE 48-QTDE-TOTAL TO WRK-QTDE-TOTAL
              IF 48-QTDE-TOTAL GREATER   1
                 MOVE   '0768'      TO  ROTSAI-COD-RETORNO
                 MOVE   SG-MENSAGEM(0768)
                                    TO  ROTSAI-MSG-RETORNO
                 PERFORM  70000-FINALIZAR
              END-IF
           ELSE
              MOVE   '0699'         TO  ROTSAI-COD-RETORNO
              MOVE   SG-MENSAGEM(0675)
                                    TO  ROTSAI-MSG-RETORNO
              PERFORM  70000-FINALIZAR
           END-IF.

4S2511*    IF ROTENT-CCNPJ-CPF-PROPN-ANT  EQUAL ZEROS
4S2511     IF ROTENT-CCNPJ-CPF-PROPN-ANT  EQUAL SPACES OR
4S2511                                     LOW-VALUES
              PERFORM  50000-RECUPERA-SACADO-ANT
           ELSE
              PERFORM  51000-RECUPERA-PROPONENTE-ANT
              PERFORM  52000-RECUPERA-PESSOA-LOG
           END-IF.

      *----------------------------------------------------------------*
       42000-99-FIM.                   EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       42500-VALIDAR-PROPONENTE         SECTION.
      *----------------------------------------------------------------*

           MOVE  'PARCIAL-CT'             TO 56-INSTRUCAO.
4S2511*    MOVE  ROTENT-CCNPJ-CPF         TO 56-CCNPJ-CPF.
4S2511     MOVE  ROTENT-CCNPJ-CPF         TO 56-CCNPJ-CPF-ST.
4S2511*    MOVE  ROTENT-CFLIAL-CNPJ       TO 56-CFLIAL-CNPJ.
4S2511     MOVE  ROTENT-CFLIAL-CNPJ       TO 56-CFLIAL-CNPJ-ST.
4S2511*    MOVE  ROTENT-CCNPJ-CPF-PROPN   TO 56-CCNPJ-CPF-PROPN.
4S2511     MOVE  ROTENT-CCNPJ-CPF-PROPN   TO 56-CCNPJ-CPF-PROPN-ST.
4S2511*    MOVE  ROTENT-CFLIAL-CNPJ-PROPN TO 56-CFLIAL-CNPJ-PROPN.
4S2511     MOVE  ROTENT-CFLIAL-CNPJ-PROPN TO 56-CFLIAL-CNPJ-PROPN-ST.
           MOVE  ROTENT-DINIC-VGCIA-ISENC TO 56-DINIC-VGCIA-ISENC.
           MOVE  ROTENT-DFIM-VGCIA-ISENC  TO 56-DFIM-VGCIA-ISENC.
           MOVE  ROTENT-CTPO-ISENC-DESC   TO 56-CTPO-ISENC-DESC.

           MOVE  'DCOM6780'               TO WRK-MODULO.

           CALL  WRK-MODULO            USING
                                       56-ARGUMENTOS-ENTRADA
                                       56-RETORNO
                                       LKG-ERRO-AREA
                                       LKG-SQLCA.

           IF 56-COD-RETORNO EQUAL ZEROS
              MOVE 56-QTDE-TOTAL TO WRK-QTDE-TOTAL
              IF 56-QTDE-TOTAL GREATER   1
                 MOVE   '0768'      TO  ROTSAI-COD-RETORNO
                 MOVE   SG-MENSAGEM(0768)
                                    TO  ROTSAI-MSG-RETORNO
                 PERFORM  70000-FINALIZAR
              END-IF
           ELSE
              IF 56-COD-RETORNO NOT EQUAL 3
                 MOVE   '0699'         TO  ROTSAI-COD-RETORNO
                 MOVE   SG-MENSAGEM(0675)
                                       TO  ROTSAI-MSG-RETORNO
                 PERFORM  70000-FINALIZAR
              END-IF
           END-IF.

4S2511*    IF ROTENT-CCNPJ-CPF-PROPN-ANT  EQUAL ZEROS
4S2511     IF ROTENT-CCNPJ-CPF-PROPN-ANT  EQUAL SPACES OR
4S2511                                     LOW-VALUES
              PERFORM  50000-RECUPERA-SACADO-ANT
           ELSE
              PERFORM  51000-RECUPERA-PROPONENTE-ANT
              PERFORM  52000-RECUPERA-PESSOA-LOG
           END-IF.

      *----------------------------------------------------------------*
       42500-99-FIM.                   EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       42750-VALIDA-ELMTO-COUNT         SECTION.
      *----------------------------------------------------------------*

           IF WRK-QTDE-TOTAL EQUAL 1
              IF (ROTENT-CCNPJ-CPF-PROPN-ANT   EQUAL
                  ROTENT-CCNPJ-CPF-PROPN) AND
                 (ROTENT-CFLIAL-CNPJ-PROPN-ANT EQUAL
                  ROTENT-CFLIAL-CNPJ-PROPN)
                 IF (WRK-DATA-FIM-ANT >= WRK-DATA-INI AND
                     WRK-DATA-INI-ANT <= WRK-DATA-FIM)
                     NEXT SENTENCE
                 ELSE
                     MOVE '0768'      TO  ROTSAI-COD-RETORNO
                     MOVE SG-MENSAGEM(0768)
                                      TO  ROTSAI-MSG-RETORNO
                     PERFORM  70000-FINALIZAR
                  END-IF
              ELSE
                  MOVE   '0768'      TO  ROTSAI-COD-RETORNO
                  MOVE   SG-MENSAGEM(0768)
                                     TO  ROTSAI-MSG-RETORNO
                  PERFORM  70000-FINALIZAR
              END-IF
           END-IF.

      *----------------------------------------------------------------*
       42750-99-FIM.                   EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       43000-ENCERRAR-ISENCOES-ANT      SECTION.
      *----------------------------------------------------------------*

4S2511*    MOVE  ROTENT-CCNPJ-CPF         TO 48-CCNPJ-CPF.
4S2511     MOVE  ROTENT-CCNPJ-CPF         TO 48-CCNPJ-CPF-ST.
4S2511*    MOVE  ROTENT-CFLIAL-CNPJ       TO 48-CFLIAL-CNPJ.
4S2511     MOVE  ROTENT-CFLIAL-CNPJ       TO 48-CFLIAL-CNPJ-ST.
           MOVE  ROTENT-CTPO-ISENC-DESC   TO 48-CTPO-ISENC-DESC.
           MOVE  ROTENT-DINIC-VGCIA-ISENC-ANT
                                          TO 48-DINIC-VGCIA-ISENC.

           MOVE  'DCOM6740'               TO WRK-MODULO.

           CALL  WRK-MODULO            USING
                                       48-ARGUMENTOS-ENTRADA
                                       48-RETORNO
                                       LKG-ERRO-AREA
                                       LKG-SQLCA.

           IF 48-COD-RETORNO EQUAL ZEROS OR 2 OR 3
              MOVE 48-QTDE-TOTAL TO WRK-QTDE-TOTAL
              IF 48-QTDE-TOTAL GREATER   1
                 MOVE   '0768'      TO  ROTSAI-COD-RETORNO
                 MOVE   SG-MENSAGEM(0768)
                                    TO  ROTSAI-MSG-RETORNO
                 PERFORM  70000-FINALIZAR
              END-IF
           ELSE
              MOVE   '0699'         TO  ROTSAI-COD-RETORNO
              MOVE   SG-MENSAGEM(0675)
                                    TO  ROTSAI-MSG-RETORNO
              PERFORM  70000-FINALIZAR
           END-IF.

           MOVE LENGTH OF 48-OCORRENCIAS(1)
                                      TO X1-WREG-LOG-ANTES-LEN.
           MOVE 48-OCORRENCIAS(1)     TO X1-WREG-LOG-ANTES-TEXT.

           PERFORM  43100-ENCERRAR-PESSOA.

4S2511*    IF  ROTENT-CCNPJ-CPF-PROPN-ANT NOT EQUAL ZEROS
4S2511     IF  ROTENT-CCNPJ-CPF-PROPN-ANT NOT EQUAL SPACES AND
4S2511                                      NOT EQUAL LOW-VALUES
               PERFORM  43200-ENCERRAR-SACADO
           END-IF.

      *----------------------------------------------------------------*
       43000-99-FIM.                   EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       43100-ENCERRAR-PESSOA           SECTION.
      *----------------------------------------------------------------*

4S2511*    MOVE  ROTENT-CCNPJ-CPF             TO  48-CCNPJ-CPF.
4S2511     MOVE  ROTENT-CCNPJ-CPF             TO  48-CCNPJ-CPF-ST.
           MOVE  WRK-NSEQ-PSSOA-DESC          TO  48-NSEQ-PSSOA-DESC.
           MOVE  ROTENT-DINIC-VGCIA-ISENC-ANT TO  48-DINIC-VGCIA-ISENC.
           MOVE  ROTENT-CTPO-ISENC-DESC       TO  48-CTPO-ISENC-DESC.
           MOVE  WRK-DATA-ATU(1:4)        TO  48-DFIM-VGCIA-ISENC(7:4).
           MOVE  '.'                      TO  48-DFIM-VGCIA-ISENC(6:1).
           MOVE  WRK-DATA-ATU(5:2)        TO  48-DFIM-VGCIA-ISENC(4:2).
           MOVE  '.'                      TO  48-DFIM-VGCIA-ISENC(3:1).
           MOVE  WRK-DATA-ATU(7:2)        TO  48-DFIM-VGCIA-ISENC(1:2).
4S2511*    MOVE  ROTENT-CFLIAL-CNPJ           TO  48-CFLIAL-CNPJ.
4S2511     MOVE  ROTENT-CFLIAL-CNPJ           TO  48-CFLIAL-CNPJ-ST.
4S2511*    MOVE  ROTENT-CCTRL-CNPJ-CPF        TO  48-CCTRL-CNPJ-CPF.
4S2511     MOVE  ROTENT-CCTRL-CNPJ-CPF        TO  48-CCTRL-CNPJ-CPF-ST.
           MOVE  1                            TO  48-CCLUB.
4S2511*    IF  ROTENT-CCNPJ-CPF-PROPN-ANT     EQUAL ZEROS
4S2511     IF  ROTENT-CCNPJ-CPF-PROPN-ANT     EQUAL SPACES OR
4S2511                                     LOW-VALUES
               MOVE  005                      TO  48-CABRGC-ISENC-DESC
               MOVE  'N'                      TO  48-CINDCD-REST-CDENT
           ELSE
               MOVE  006                      TO  48-CABRGC-ISENC-DESC
               MOVE  'S'                      TO  48-CINDCD-REST-CDENT
           END-IF

4S2511*    IF  ROTENT-CFLIAL-CNPJ             EQUAL  ZEROS
4S2511     IF  ROTENT-CFLIAL-CNPJ             EQUAL  '0000' OR
4S2511                                      EQUAL  SPACES OR
4S2511                                      EQUAL  LOW-VALUES
               MOVE  'FI'                     TO  48-CINDCD-PSSOA-DESC
           ELSE
               MOVE  'PJ'                     TO  48-CINDCD-PSSOA-DESC
           END-IF.

           MOVE  ROTENT-CFUNC-BDSCO           TO  48-CFUNC-BDSCO.
           MOVE  ROTENT-CTERM                 TO  48-CTERM.
           MOVE  'TOTAL'                      TO  48-INSTRUCAO.
           MOVE  'DCOM6743'                   TO  WRK-MODULO.

           CALL  WRK-MODULO         USING  48-ARGUMENTOS-ENTRADA
                                           48-RETORNO
                                           LKG-ERRO-AREA
                                           LKG-SQLCA.

           IF  48-COD-RETORNO NOT EQUAL ZEROS
               IF  48-COD-RETORNO EQUAL 3
                   MOVE '0769'         TO  ROTSAI-COD-RETORNO
                   MOVE SG-MENSAGEM(0769)
                                       TO  ROTSAI-MSG-RETORNO
               ELSE
                   MOVE '0699'         TO  ROTSAI-COD-RETORNO
                   MOVE SG-MENSAGEM(0770)
                                       TO  ROTSAI-MSG-RETORNO
               END-IF 
               PERFORM  70000-FINALIZAR
           END-IF.

      *----------------------------------------------------------------*
       43100-99-FIM.                   EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       43200-ENCERRAR-SACADO             SECTION.
      *----------------------------------------------------------------*

4S2511*    MOVE  ROTENT-CCNPJ-CPF      TO  56-CCNPJ-CPF.
4S2511     MOVE  ROTENT-CCNPJ-CPF      TO  56-CCNPJ-CPF-ST.
4S2511*    MOVE  ROTENT-CFLIAL-CNPJ    TO  56-CFLIAL-CNPJ.
4S2511     MOVE  ROTENT-CFLIAL-CNPJ    TO  56-CFLIAL-CNPJ-ST.
4S2511*    MOVE  ROTENT-CCTRL-CNPJ-CPF TO  56-CCTRL-CNPJ-CPF.
4S2511     MOVE  ROTENT-CCTRL-CNPJ-CPF TO  56-CCTRL-CNPJ-CPF-ST.
           MOVE  ROTENT-DINIC-VGCIA-ISENC-ANT
                                       TO  56-DINIC-VGCIA-ISENC.
           MOVE  WRK-DATA-ATU(1:4)     TO  56-DFIM-VGCIA-ISENC(7:4).
           MOVE  '.'                   TO  56-DFIM-VGCIA-ISENC(6:1).
           MOVE  WRK-DATA-ATU(5:2)     TO  56-DFIM-VGCIA-ISENC(4:2).
           MOVE  '.'                   TO  56-DFIM-VGCIA-ISENC(3:1).
           MOVE  WRK-DATA-ATU(7:2)     TO  56-DFIM-VGCIA-ISENC(1:2).
           MOVE  ROTENT-CTPO-ISENC-DESC
                                       TO  56-CTPO-ISENC-DESC.
           MOVE  WRK-NSEQ-PSSOA-DESC
                                       TO  56-NSEQ-PSSOA-DESC.
           MOVE  1                     TO  56-CCLUB-CDENT-PROPN.
4S2511*    MOVE  ROTENT-CCNPJ-CPF-PROPN-ANT
4S2511*                                TO  56-CCNPJ-CPF-PROPN.
4S2511     MOVE  ROTENT-CCNPJ-CPF-PROPN-ANT
4S2511                                TO  56-CCNPJ-CPF-PROPN-ST.
4S2511*    MOVE  ROTENT-CFLIAL-CNPJ-PROPN-ANT
4S2511*                                TO  56-CFLIAL-CNPJ-PROPN.
4S2511     MOVE  ROTENT-CFLIAL-CNPJ-PROPN-ANT
4S2511                                TO  56-CFLIAL-CNPJ-PROPN-ST.
           MOVE  ROTENT-CCTRL-CNPJ-PROPN-ANT
                                       TO  56-CCTRL-CNPJ-PROPN.
           MOVE  ROTENT-CFUNC-BDSCO    TO  56-CFUNC-BDSCO.
           MOVE  ROTENT-CTERM          TO  56-CTERM.
           MOVE  WRK-TIMESTAMP-ATUAL   TO  56-HULT-ATULZ.
           MOVE  'TOTAL'               TO  56-INSTRUCAO.
           MOVE  'DCOM6783'            TO  WRK-MODULO.

           CALL  WRK-MODULO         USING  56-ARGUMENTOS-ENTRADA
                                           56-RETORNO
                                           LKG-ERRO-AREA
                                           LKG-SQLCA.

           IF  56-COD-RETORNO          NOT  EQUAL  ZEROS
               IF  56-COD-RETORNO EQUAL 3
                   MOVE '0771'         TO  ROTSAI-COD-RETORNO
                   MOVE SG-MENSAGEM(0771)
                                       TO  ROTSAI-MSG-RETORNO
               ELSE
                   MOVE '0699'         TO  ROTSAI-COD-RETORNO
                   MOVE SG-MENSAGEM(0772)
                                       TO  ROTSAI-MSG-RETORNO
               END-IF
               PERFORM  70000-FINALIZAR
           END-IF.

      *----------------------------------------------------------------*
       43200-99-FIM.                   EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       43500-SOMA-DIA                  SECTION.
      *----------------------------------------------------------------*

           MOVE  WRK-DATA-ATU          TO   WRK-DT-ENT.

           CALL 'BRAD0025' USING  WRK-DT-ENT  WRK-NUM-DIAS WRK-DT-SAI.

           IF    RETURN-CODE NOT EQUAL ZEROS
                 MOVE SG-MENSAGEM(0714)           TO ROTSAI-MSG-RETORNO
                 MOVE  0999                       TO ROTSAI-COD-RETORNO
                 PERFORM  70000-FINALIZAR
           END-IF.

           MOVE WRK-DT-SAI             TO   WRK-DATA-ATU-1.

           MOVE  WRK-DATA-ATU-1(1:4)   TO   WRK-DINIC-VGCIA-ISENC(7:4).
           MOVE  '.'                   TO   WRK-DINIC-VGCIA-ISENC(6:1).
           MOVE  WRK-DATA-ATU-1(5:2)   TO   WRK-DINIC-VGCIA-ISENC(4:2).
           MOVE  '.'                   TO   WRK-DINIC-VGCIA-ISENC(3:1).
           MOVE  WRK-DATA-ATU-1(7:2)   TO   WRK-DINIC-VGCIA-ISENC(1:2).

      *----------------------------------------------------------------*
       43500-99-FIM.                   EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       44000-EXCLUIR-ISENCOES-ANT      SECTION.
      *----------------------------------------------------------------*

4S2511*    IF  ROTENT-CCNPJ-CPF-PROPN-ANT EQUAL ZEROS
4S2511     IF  ROTENT-CCNPJ-CPF-PROPN-ANT EQUAL SPACES OR
4S2511                                     LOW-VALUES
               MOVE  005               TO  48-CABRGC-ISENC-DESC
               PERFORM  44100-EXCLUIR-ISENCAO
           ELSE
               PERFORM  44100-EXCLUIR-ISENCAO
               PERFORM  44200-EXCLUIR-SACADO
           END-IF.

      *----------------------------------------------------------------*
       44000-99-FIM.                   EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       44100-EXCLUIR-ISENCAO            SECTION.
      *----------------------------------------------------------------*


4S2511*    MOVE  ROTENT-CCNPJ-CPF             TO  48-CCNPJ-CPF.
4S2511     MOVE  ROTENT-CCNPJ-CPF             TO  48-CCNPJ-CPF-ST.
           MOVE  WRK-NSEQ-PSSOA-DESC          TO  48-NSEQ-PSSOA-DESC.
           MOVE  ROTENT-DINIC-VGCIA-ISENC-ANT TO  48-DINIC-VGCIA-ISENC.
           MOVE  ROTENT-CTPO-ISENC-DESC       TO  48-CTPO-ISENC-DESC.
4S2511*    MOVE  ROTENT-CFLIAL-CNPJ           TO  48-CFLIAL-CNPJ.
4S2511     MOVE  ROTENT-CFLIAL-CNPJ           TO  48-CFLIAL-CNPJ-ST.
4S2511*    MOVE  ROTENT-CCTRL-CNPJ-CPF        TO  48-CCTRL-CNPJ-CPF.
4S2511     MOVE  ROTENT-CCTRL-CNPJ-CPF        TO  48-CCTRL-CNPJ-CPF-ST.
           MOVE  'PARCIAL-02'                 TO  48-INSTRUCAO
           MOVE  'DCOM6744'                   TO  WRK-MODULO.

           CALL  WRK-MODULO         USING     48-ARGUMENTOS-ENTRADA
                                              48-RETORNO
                                              LKG-ERRO-AREA
                                              LKG-SQLCA.

           IF  48-COD-RETORNO          NOT  EQUAL  ZEROS
               IF  48-COD-RETORNO EQUAL 3
                   MOVE '0773'         TO  ROTSAI-COD-RETORNO
                   MOVE SG-MENSAGEM(773)
                                       TO  ROTSAI-MSG-RETORNO
               ELSE
                   MOVE '0699'         TO  ROTSAI-COD-RETORNO
                   MOVE SG-MENSAGEM(774)
                                       TO  ROTSAI-MSG-RETORNO
               END-IF
               PERFORM  70000-FINALIZAR
           END-IF.

      *----------------------------------------------------------------*
       44100-99-FIM.                   EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       44200-EXCLUIR-SACADO             SECTION.
      *----------------------------------------------------------------*

4S2511*    MOVE  ROTENT-CCNPJ-CPF      TO  56-CCNPJ-CPF.
4S2511     MOVE  ROTENT-CCNPJ-CPF      TO  56-CCNPJ-CPF-ST.
4S2511*    MOVE  ROTENT-CFLIAL-CNPJ    TO  56-CFLIAL-CNPJ.
4S2511     MOVE  ROTENT-CFLIAL-CNPJ    TO  56-CFLIAL-CNPJ-ST.
4S2511*    MOVE  ROTENT-CCTRL-CNPJ-CPF TO  56-CCTRL-CNPJ-CPF.
4S2511     MOVE  ROTENT-CCTRL-CNPJ-CPF TO  56-CCTRL-CNPJ-CPF-ST.
           MOVE  ROTENT-DINIC-VGCIA-ISENC-ANT
                                       TO  56-DINIC-VGCIA-ISENC.
           MOVE  ROTENT-CTPO-ISENC-DESC
                                       TO  56-CTPO-ISENC-DESC.
           MOVE  WRK-NSEQ-PSSOA-DESC
                                       TO  56-NSEQ-PSSOA-DESC.
4S2511*    MOVE  ROTENT-CCNPJ-CPF-PROPN-ANT
4S2511*                                TO  56-CCNPJ-CPF-PROPN.
4S2511     MOVE  ROTENT-CCNPJ-CPF-PROPN-ANT
4S2511                                TO  56-CCNPJ-CPF-PROPN-ST.
           MOVE  'DCOM6784'            TO  WRK-MODULO.

           CALL  WRK-MODULO         USING  56-ARGUMENTOS-ENTRADA
                                           56-RETORNO
                                           LKG-ERRO-AREA
                                           LKG-SQLCA.

           IF  56-COD-RETORNO          NOT  EQUAL  ZEROS
               IF  56-COD-RETORNO EQUAL 3
                   MOVE '0775'         TO  ROTSAI-COD-RETORNO
                   MOVE SG-MENSAGEM(775)
                                       TO  ROTSAI-MSG-RETORNO
               ELSE
                   MOVE '0699'         TO  ROTSAI-COD-RETORNO
                   MOVE SG-MENSAGEM(776)
                                       TO  ROTSAI-MSG-RETORNO
               END-IF
               PERFORM  70000-FINALIZAR
           END-IF.

       44200-99-FIM.                   EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       45000-INCLUIR-ISENCOES          SECTION.
      *----------------------------------------------------------------*


           PERFORM 45050-OBTER-SEQUENCIA.

           IF  ROTENT-CINDCD-REST-CDENT EQUAL  'N'
               MOVE  005                TO  48-CABRGC-ISENC-DESC
               PERFORM  45100-INCLUIR-ISENCAO
           ELSE
               MOVE  006                TO  48-CABRGC-ISENC-DESC
               PERFORM  45100-INCLUIR-ISENCAO
               PERFORM  45200-INCLUIR-SACADO
           END-IF.

           PERFORM  60000-GRAVAR-LOG.

      *----------------------------------------------------------------*
       45000-99-FIM.                   EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       45050-OBTER-SEQUENCIA           SECTION.
      *----------------------------------------------------------------*

           MOVE  ROTENT-COD-RETORNO    TO  WRK-COD-RETORNO-E.
           MOVE  ROTENT-MSG-RETORNO    TO  WRK-MSG-RETORNO-E.
           MOVE  ROTENT-RESTART        TO  WRK-RESTART-E.
           MOVE  ROTENT-FLAG           TO  WRK-FLAG-E.
           MOVE  ROTENT-CFUNC-BDSCO    TO  WRK-CFUNC-BDSCO-E.
           MOVE  ROTENT-CTERM          TO  WRK-CTERM-E.
           MOVE  13                    TO  WRK-CELMTO-DESC-COML-E.
           MOVE  001                   TO  WRK-WCHAVE-PRINC-ELMTO-E.
           MOVE  003                   TO  WRK-QDIG-SEQ-ELMTO-E.
           MOVE  'DCOM5022'            TO  WRK-MODULO.

           CALL  WRK-MODULO         USING  WRK-ENTRADA
                                           WRK-SAIDA
                                           LKG-ERRO-AREA
                                           LKG-SQLCA.

           IF  WRK-COD-RETORNO-S  NOT  EQUAL  ZEROS
               MOVE  '0777'               TO  ROTSAI-COD-RETORNO
               MOVE  SG-MENSAGEM(777)
                                          TO  ROTSAI-MSG-RETORNO
               PERFORM  70000-FINALIZAR
           END-IF.

           MOVE WRK-NULT-SEQ-ELMTO-S  TO WRK-NSEQ-PSSOA-DESC.

      *----------------------------------------------------------------*
       45050-99-FIM.                   EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       45100-INCLUIR-ISENCAO            SECTION.
      *----------------------------------------------------------------*

4S2511*    MOVE  ROTENT-CCNPJ-CPF      TO  48-CCNPJ-CPF.
4S2511     MOVE  ROTENT-CCNPJ-CPF      TO  48-CCNPJ-CPF-ST.
           MOVE  WRK-NSEQ-PSSOA-DESC
                                       TO  48-NSEQ-PSSOA-DESC.
           MOVE  WRK-DINIC-VGCIA-ISENC
                                       TO  48-DINIC-VGCIA-ISENC.
           MOVE  ROTENT-CTPO-ISENC-DESC
                                       TO  48-CTPO-ISENC-DESC.
           MOVE  ROTENT-DFIM-VGCIA-ISENC
                                       TO  48-DFIM-VGCIA-ISENC.
4S2511*    MOVE  ROTENT-CFLIAL-CNPJ    TO  48-CFLIAL-CNPJ.
4S2511     MOVE  ROTENT-CFLIAL-CNPJ    TO  48-CFLIAL-CNPJ-ST.
           MOVE  ROTENT-CCTRL-CNPJ-CPF TO  48-CCTRL-CNPJ-CPF.
4S2511     MOVE  ROTENT-CCTRL-CNPJ-CPF TO  48-CCTRL-CNPJ-CPF-ST.
           MOVE  1                     TO  48-CCLUB.

4S2511*    IF  ROTENT-CFLIAL-CNPJ      EQUAL  ZEROS
4S2511     IF  ROTENT-CFLIAL-CNPJ      EQUAL  '0000' OR
4S2511                                   EQUAL  SPACES OR
4S2511                                   EQUAL  LOW-VALUES
               MOVE  'FI'              TO  48-CINDCD-PSSOA-DESC
           ELSE
               MOVE  'PJ'              TO  48-CINDCD-PSSOA-DESC
           END-IF.

           MOVE  ROTENT-CINDCD-REST-CDENT TO  48-CINDCD-REST-CDENT.
           MOVE  ROTENT-CFUNC-BDSCO       TO  48-CFUNC-BDSCO.
           MOVE  ROTENT-CTERM             TO  48-CTERM.
           MOVE  'DCOM6742'            TO  WRK-MODULO.

           CALL  WRK-MODULO         USING  48-ARGUMENTOS-ENTRADA
                                           48-RETORNO
                                           LKG-ERRO-AREA
                                           LKG-SQLCA.

           IF  48-COD-RETORNO          NOT  EQUAL  ZEROS
               IF  48-COD-RETORNO EQUAL 4
                         MOVE '0778'         TO  ROTSAI-COD-RETORNO
                         MOVE SG-MENSAGEM(778)
                                          TO  ROTSAI-MSG-RETORNO
                         PERFORM  70000-FINALIZAR
               ELSE
                   MOVE '0699'         TO  ROTSAI-COD-RETORNO
                   MOVE SG-MENSAGEM(779)
                                       TO  ROTSAI-MSG-RETORNO
                   PERFORM  70000-FINALIZAR
               END-IF
           END-IF.

      *----------------------------------------------------------------*
       45100-99-FIM.                   EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       45200-INCLUIR-SACADO             SECTION.
      *----------------------------------------------------------------*

4S2511*    MOVE  ROTENT-CCNPJ-CPF      TO  56-CCNPJ-CPF.
4S2511     MOVE  ROTENT-CCNPJ-CPF      TO  56-CCNPJ-CPF-ST.
4S2511*    MOVE  ROTENT-CFLIAL-CNPJ    TO  56-CFLIAL-CNPJ.
4S2511     MOVE  ROTENT-CFLIAL-CNPJ    TO  56-CFLIAL-CNPJ-ST.
           MOVE  ROTENT-CCTRL-CNPJ-CPF TO  56-CCTRL-CNPJ-CPF.
4S2511     MOVE  ROTENT-CCTRL-CNPJ-CPF TO  56-CCTRL-CNPJ-CPF-ST.
           MOVE  WRK-DINIC-VGCIA-ISENC
                                       TO  56-DINIC-VGCIA-ISENC.
           MOVE  ROTENT-DFIM-VGCIA-ISENC
                                       TO  56-DFIM-VGCIA-ISENC.
           MOVE  ROTENT-CTPO-ISENC-DESC
                                       TO  56-CTPO-ISENC-DESC.
           MOVE  WRK-NSEQ-PSSOA-DESC
                                       TO  56-NSEQ-PSSOA-DESC.
           MOVE  1                     TO  56-CCLUB-CDENT-PROPN.
4S2511*    MOVE  ROTENT-CCNPJ-CPF-PROPN
4S2511*                                TO  56-CCNPJ-CPF-PROPN.
4S2511     MOVE  ROTENT-CCNPJ-CPF-PROPN
4S2511                                TO  56-CCNPJ-CPF-PROPN-ST.
4S2511*    MOVE  ROTENT-CFLIAL-CNPJ-PROPN
4S2511*                                TO  56-CFLIAL-CNPJ-PROPN.
4S2511     MOVE  ROTENT-CFLIAL-CNPJ-PROPN
4S2511                                TO  56-CFLIAL-CNPJ-PROPN-ST.
           MOVE  ROTENT-CCTRL-CNPJ-PROPN
                                       TO  56-CCTRL-CNPJ-PROPN.
           MOVE  ROTENT-CFUNC-BDSCO    TO  56-CFUNC-BDSCO.
           MOVE  ROTENT-CTERM          TO  56-CTERM.
           MOVE  'DCOM6782'            TO  WRK-MODULO.

           CALL  WRK-MODULO         USING  56-ARGUMENTOS-ENTRADA
                                           56-RETORNO
                                           LKG-ERRO-AREA
                                           LKG-SQLCA.

           IF  56-COD-RETORNO          NOT  EQUAL  ZEROS
               IF  56-COD-RETORNO EQUAL 4
                   MOVE '0780'         TO  ROTSAI-COD-RETORNO
                   MOVE SG-MENSAGEM(780)
                                       TO  ROTSAI-MSG-RETORNO
               ELSE
                   MOVE '0699'         TO  ROTSAI-COD-RETORNO
                   MOVE SG-MENSAGEM(781)
                                       TO  ROTSAI-MSG-RETORNO
               END-IF
               PERFORM  70000-FINALIZAR
           END-IF.

      *----------------------------------------------------------------*
       45200-99-FIM.                   EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       50000-RECUPERA-SACADO-ANT       SECTION.
      *----------------------------------------------------------------*

           MOVE  'PARCIAL-01'             TO 48-INSTRUCAO.
4S2511*    MOVE  ROTENT-CCNPJ-CPF         TO 48-CCNPJ-CPF.
4S2511     MOVE  ROTENT-CCNPJ-CPF         TO 48-CCNPJ-CPF-ST.
4S2511*    MOVE  ROTENT-CFLIAL-CNPJ       TO 48-CFLIAL-CNPJ.
4S2511     MOVE  ROTENT-CFLIAL-CNPJ       TO 48-CFLIAL-CNPJ-ST.
           MOVE  ROTENT-CCTRL-CNPJ-CPF    TO 48-CCTRL-CNPJ-CPF.
4S2511     MOVE  ROTENT-CCTRL-CNPJ-CPF    TO 48-CCTRL-CNPJ-CPF-ST.
           MOVE  ROTENT-DINIC-VGCIA-ISENC-ANT
                                          TO 48-DINIC-VGCIA-ISENC.
           MOVE  ROTENT-CTPO-ISENC-DESC   TO 48-CTPO-ISENC-DESC.
           MOVE 005                       TO 48-CABRGC-ISENC-DESC.

           MOVE  'DCOM6740'               TO WRK-MODULO.

           CALL  WRK-MODULO            USING
                                       48-ARGUMENTOS-ENTRADA
                                       48-RETORNO
                                       LKG-ERRO-AREA
                                       LKG-SQLCA.

           IF 48-COD-RETORNO NOT EQUAL ZEROS
              MOVE   '0699'         TO  ROTSAI-COD-RETORNO
              MOVE   SG-MENSAGEM(782)
                                    TO  ROTSAI-MSG-RETORNO
              PERFORM  70000-FINALIZAR
           END-IF.

           MOVE 48-OCORRENCIAS(1)         TO  WRK-48-OCORRENCIAS.


           MOVE 48RT-NSEQ-PSSOA-DESC(1)   TO  WRK-NSEQ-PSSOA-DESC.

           MOVE 48RT-NSEQ-PSSOA-DESC(1)   TO  WRK-NSEQ-PSSOA-DESC-ANT.

           MOVE   LENGTH OF WRK-48-OCORRENCIAS
                                          TO X1-WREG-LOG-ANTES-LEN
           MOVE   WRK-48-OCORRENCIAS      TO X1-WREG-LOG-ANTES-TEXT.

      *----------------------------------------------------------------*
       50000-99-FIM.                   EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       51000-RECUPERA-PROPONENTE-ANT    SECTION.
      *----------------------------------------------------------------*

           MOVE  'PARCIAL-01'                 TO 56-INSTRUCAO.
4S2511*    MOVE  ROTENT-CCNPJ-CPF             TO 56-CCNPJ-CPF.
4S2511     MOVE  ROTENT-CCNPJ-CPF             TO 56-CCNPJ-CPF-ST.
4S2511*    MOVE  ROTENT-CFLIAL-CNPJ           TO 56-CFLIAL-CNPJ.
4S2511     MOVE  ROTENT-CFLIAL-CNPJ           TO 56-CFLIAL-CNPJ-ST.
4S2511*    MOVE  ROTENT-CCNPJ-CPF-PROPN-ANT   TO 56-CCNPJ-CPF-PROPN.
4S2511     MOVE  ROTENT-CCNPJ-CPF-PROPN-ANT   TO 56-CCNPJ-CPF-PROPN-ST.
4S2511*    MOVE  ROTENT-CFLIAL-CNPJ-PROPN-ANT TO 56-CFLIAL-CNPJ-PROPN.
4S2511     MOVE  ROTENT-CFLIAL-CNPJ-PROPN-ANT TO 56-CFLIAL-CNPJ-PROPN-ST.
           MOVE  ROTENT-DINIC-VGCIA-ISENC-ANT TO 56-DINIC-VGCIA-ISENC.
           MOVE  ROTENT-CTPO-ISENC-DESC       TO 56-CTPO-ISENC-DESC.

           MOVE  'DCOM6780'                   TO WRK-MODULO.

           CALL  WRK-MODULO            USING
                                       56-ARGUMENTOS-ENTRADA
                                       56-RETORNO
                                       LKG-ERRO-AREA
                                       LKG-SQLCA.

           IF 56-COD-RETORNO NOT EQUAL ZEROS
              MOVE   '0699'         TO  ROTSAI-COD-RETORNO
              MOVE   SG-MENSAGEM(783)
                                    TO  ROTSAI-MSG-RETORNO
              PERFORM  70000-FINALIZAR
           END-IF.

           MOVE 56RT-NSEQ-PSSOA-DESC(1)   TO  WRK-NSEQ-PSSOA-DESC.

      *----------------------------------------------------------------*
       51000-99-FIM.                   EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       52000-RECUPERA-PESSOA-LOG       SECTION.
      *----------------------------------------------------------------*

4S2511*    MOVE  ROTENT-CCNPJ-CPF         TO 48-CCNPJ-CPF.
4S2511     MOVE  ROTENT-CCNPJ-CPF         TO 48-CCNPJ-CPF-ST.
4S2511*    MOVE  ROTENT-CFLIAL-CNPJ       TO 48-CFLIAL-CNPJ.
4S2511     MOVE  ROTENT-CFLIAL-CNPJ       TO 48-CFLIAL-CNPJ-ST.
           MOVE  ROTENT-CCTRL-CNPJ-CPF    TO 48-CCTRL-CNPJ-CPF.
4S2511     MOVE  ROTENT-CCTRL-CNPJ-CPF    TO 48-CCTRL-CNPJ-CPF-ST.
           MOVE  WRK-NSEQ-PSSOA-DESC      TO 48-NSEQ-PSSOA-DESC.
           MOVE  ROTENT-DINIC-VGCIA-ISENC-ANT
                                          TO 48-DINIC-VGCIA-ISENC.
           MOVE  ROTENT-CTPO-ISENC-DESC   TO 48-CTPO-ISENC-DESC.
           MOVE 005                       TO 48-CABRGC-ISENC-DESC.

           MOVE  'DCOM6740'               TO WRK-MODULO.

           CALL  WRK-MODULO            USING
                                       48-ARGUMENTOS-ENTRADA
                                       48-RETORNO
                                       LKG-ERRO-AREA
                                       LKG-SQLCA.

           IF 48-COD-RETORNO NOT EQUAL ZEROS
              MOVE   '0699'         TO  ROTSAI-COD-RETORNO
              MOVE   SG-MENSAGEM(782)
                                    TO  ROTSAI-MSG-RETORNO
              PERFORM  70000-FINALIZAR
           END-IF.

           MOVE 48-OCORRENCIAS(1)         TO  WRK-48-OCORRENCIAS.

      *----------------------------------------------------------------*
       52000-99-FIM.                   EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       60000-GRAVAR-LOG                SECTION.
      *----------------------------------------------------------------*

           MOVE  002                       TO  X1-CELMTO-DESC-COML.
           MOVE  059                       TO  X1-CEVNTO-LOG-DESC.
           MOVE  'A'                       TO  X1-CINDCD-MANUT-REG.

           MOVE  ROTENT-CCNPJ-CPF          TO  WRK-CCHAVE-ELMTO-DESC-1.
           MOVE  WRK-DATA-CHAVE            TO  WRK-CCHAVE-ELMTO-DESC-2.
           MOVE  ROTENT-CTPO-ISENC-DESC    TO  WRK-CCHAVE-ELMTO-DESC-3.

           MOVE  WRK-CCHAVE-ELMTO-DESC TO  X1-CCHAVE-ELMTO-DESC.

           MOVE  LENGTH OF 48-COLUNAS-DA-TABELA
                                       TO  X1-WREG-LOG-APOS-LEN.
           MOVE  48-COLUNAS-DA-TABELA  TO  X1-WREG-LOG-APOS-TEXT.
           MOVE  'DCOM5060'            TO  X1-CTRANS-PROG-ATULZ.
           MOVE  'O'                   TO  X1-CINDCD-MODLD-PROCM.
           MOVE  237                   TO  X1-CEMPR-INC.
           MOVE  4130                  TO  X1-CDEPDC.
           MOVE  ROTENT-CFUNC-BDSCO    TO  X1-CFUNC-BDSCO.
           MOVE  ROTENT-CTERM          TO  X1-CTERM.
           MOVE  'DCOM5075'            TO  WRK-MODULO.

           CALL  WRK-MODULO         USING  X1-REGISTRO
                                           LKG-ERRO-AREA
                                           LKG-SQLCA.

           IF  X1-COD-RETORNO          NOT  EQUAL  ZEROS
               MOVE '0699'         TO  ROTSAI-COD-RETORNO
               MOVE SG-MENSAGEM(578)
                                   TO  ROTSAI-MSG-RETORNO
               PERFORM  70000-FINALIZAR
           END-IF.

      *----------------------------------------------------------------*
       60000-99-FIM.                   EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       70000-FINALIZAR                 SECTION.
      *----------------------------------------------------------------*

           IF  ROTSAI-COD-RETORNO NOT EQUAL '0000' AND '0002' AND
                                            '0003' AND '0004' AND
                                            '0005'
               NEXT SENTENCE
           ELSE
               MOVE  'DCOM5060'    TO  ERR-PGM
               MOVE  SPACES        TO  LKG-SQLCA
               MOVE  ERRO-AREA     TO  LKG-ERRO-AREA
           END-IF.

           GOBACK.

      *----------------------------------------------------------------*
       70000-FIM. EXIT.
      *----------------------------------------------------------------*
