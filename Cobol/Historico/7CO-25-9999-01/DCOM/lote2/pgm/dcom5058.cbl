      *----------------------------------------------------------------*
       IDENTIFICATION                  DIVISION.
      *----------------------------------------------------------------*

       PROGRAM-ID.                     DCOM5058.
       AUTHOR.                         VITAL.

      *----------------------------------------------------------------*
      *                       ALTRAN CONSULTORIA                       *
      *----------------------------------------------------------------*
      *                                                                *
      *     PROGRAMA     : DCOM5058                                    *
      *     PROGRAMADOR  : VITAL                     -   ALTRAN        *
      *     ANALISTA     : PAULO FERRARINI           -   ALTRAN        *
      *     DATA         : MAIO/2006                                   *
      *                                                                *
      *     OBJETIVO     : EXCLUSAO DE ISENCAO POR OPERACOES DO CLIEN- *
      *                    TE.                                         *
      *                                                                *
      *     INC'S        :                                             *
      *       I#DCOMSG   - AREA DE RETORNO DAS MENSAGENS DE ERRO       *
      *       I#DCOM36   - TABELA TOPER_ISENC_PSSOA                    *
      *       I#DCOM48   - TABELA TPSSOA_ISENC_DESC                    *
      *       I#DCOMX1   - ATUALIZA LOG                                *
      *       I#BRAD7C   - AREA PARA TRATAMENTO DE ERROS DB2           *
      *                                                                *
      *     MODULOS      :                                             *
      *       POOL7100   - TRATAMENTO DE ERROS                         *
      *       DCOM6684   - ACESSO AOS DADOS DA TABELA DCOMB036 (DELETE)*
      *       DCOM6744   - ACESSO AOS DADOS DA TABELA DCOMB048 (DELETE)*
      *       DCOM5075   - ATUALIZAR LOG DA DELECAO REALIZADA          *
      *                                                                *
      *----------------------------------------------------------------*
      *                        A L T E R A C A O                       *
      *----------------------------------------------------------------*
      *                                                                *
      *    PROGRAMA     : DCOM5058                                     *
      *    PROGRAMADOR  : CLAUDIANA CANDIDO             - CPM/FPOLIS   *
      *    DATA         : 24/04/2007                                   *
      *                                                                *
      *    OBJETIVO     :                                              *
      *      AJUSTAR DE ACORDO COM O NOVO PADRAO BRADESCO UTILIZANDO   *
      *      O DRESSCODE/HEXAVISON.                                    *
      *                                                                *
      *================================================================*

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
           '*** INICIO WORKING-STORAGE DCOM5058 ***'.

      *----------------------------------------------------------------*
       77  FILLER                      PIC  X(020)        VALUE
           '* AREAS AUXILIARES *'.
      *----------------------------------------------------------------*

       77  WRK-MODULO                  PIC  X(008)  VALUE  SPACES.
       77  WRK-TEM-OPERACAO            PIC  X(001)  VALUE  'N'.
       77  WRK-ENC-EXC                 PIC  X(003)  VALUE  SPACES.
       77  WRK-TIMESTAMP-ATUAL         PIC  X(026)  VALUE  SPACES.
       77  WRK-DATA-INI                PIC  9(008)  VALUE  ZEROS.
       77  WRK-DATA-FIM                PIC  9(008)  VALUE  ZEROS.
       77  WRK-DATA-ATU                PIC  9(008)  VALUE  ZEROS.
       77  WRK-DFIM-VGCIA-ISENC        PIC  X(010)  VALUE  ZEROS.
       77  WRK-NSEQ-PSSOA-DESC         PIC  9(003)  VALUE  ZEROS.

      *----------------------------------------------------------------*
       77  FILLER                      PIC  X(033)        VALUE
           '* AREA INDEXADORES E CONTADORES *'.
      *----------------------------------------------------------------*

       77  IND-ENT                     PIC  9(003)        VALUE ZEROS.

      *----------------------------------------------------------------*
       77  FILLER                      PIC  X(027)        VALUE
           '* AREA DO MODULO DCOM6684 *'.
      *----------------------------------------------------------------*

       COPY 'I#DCOM36'.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(027)        VALUE
           '* AREA DO MODULO DCOM6744 *'.
      *----------------------------------------------------------------*

       COPY 'I#DCOM48'.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(027)        VALUE
           '* AREA DO MODULO DCOM7420 *'.
      *----------------------------------------------------------------*

       COPY 'I#DCOMG4'.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(027)        VALUE
           '* AREA DO MODULO DCOM7348 *'.
      *----------------------------------------------------------------*

       COPY 'I#DCOMG9'.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(027)        VALUE
           '* AREA DO MODULO DCOM5075 *'.
      *----------------------------------------------------------------*

       COPY 'I#DCOMX1'.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(029)        VALUE
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
           '*** FINAL WORKING-STORAGE DCOM5058 ***'.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       LINKAGE                         SECTION.
      *----------------------------------------------------------------*

       01  ENTRADA-ROTEADOR.
           03  ROTENT-HEADER.
               05  ROTENT-COD-RETORNO         PIC  X(004).
               05  ROTENT-MSG-RETORNO         PIC  X(079).
               05  ROTENT-RESTART             PIC  9(005).
               05  ROTENT-FLAG                PIC  X(001).
               05  ROTENT-CFUNC-BDSCO         PIC  9(009).
               05  ROTENT-CTERM               PIC  X(008).
           03  ROTENT-DADOS.
4S2511*        05  ROTENT-CCNPJ-CPF           PIC  9(009).
4S2511         05  ROTENT-CCNPJ-CPF           PIC  X(09).
4S2511*        05  ROTENT-CFLIAL-CNPJ         PIC  9(004).
4S2511         05  ROTENT-CFLIAL-CNPJ         PIC  X(04).
               05  ROTENT-CCTRL-CNPJ-CPF      PIC  9(002).
               05  ROTENT-TAB-OPER            OCCURS 70 TIMES.
                   07  ROTENT-DANO-OPER-DESC      PIC  9(004).
                   07  ROTENT-NSEQ-OPER-DESC      PIC  9(009).

       01  SAIDA-ROTEADOR.
           03  ROTSAI-HEADER.
               05  ROTSAI-COD-RETORNO         PIC  X(004).
               05  ROTSAI-MSG-RETORNO         PIC  X(079).
               05  ROTSAI-RESTART             PIC  9(005).
               05  ROTSAI-FLAG                PIC  X(001).
           03  ROTSAI-CONSISTENCIA.
               05  ROTSAI-TAMANHO-ERRO        PIC  9(002).
               05  ROTSAI-TABELA-ERROS   OCCURS   070   TIMES.
                   07  ROTSAI-CAMPOS-ERRO     PIC  9(001).

       01  LKG-ERRO-AREA                      PIC  X(107).
       01  LKG-SQLCA                          PIC  X(136).

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

           PERFORM  80000-FINALIZAR.

      *----------------------------------------------------------------*
       00000-99-FIM.                   EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       10000-INICIALIZAR               SECTION.
      *----------------------------------------------------------------*

           INITIALIZE  SAIDA-ROTEADOR
                       36-ARGUMENTOS-ENTRADA
                       48-ARGUMENTOS-ENTRADA
                       G4-ARGUMENTOS-ENTRADA
                       G9-ARGUMENTOS-ENTRADA
                       X1-ARGUMENTOS-ENTRADA
                       LKG-ERRO-AREA
                       LKG-SQLCA.

           PERFORM  11000-OBTER-TIMESTAMP.

           MOVE  1                        TO  ROTSAI-TAMANHO-ERRO.
           MOVE  '0000'                   TO  ROTSAI-COD-RETORNO.
           MOVE  SG-MENSAGEM(001)   TO  ROTSAI-MSG-RETORNO.

      *----------------------------------------------------------------*
       10000-99-FIM.                   EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       11000-OBTER-TIMESTAMP           SECTION.
      *----------------------------------------------------------------*

           MOVE  'DCOM7999'                 TO  WRK-MODULO.

           CALL  WRK-MODULO   USING   WRK-TIMESTAMP-ATUAL
                                      LKG-ERRO-AREA
                                      LKG-SQLCA.

           MOVE  WRK-TIMESTAMP-ATUAL(1:4)   TO  WRK-DATA-ATU(1:4).
           MOVE  WRK-TIMESTAMP-ATUAL(6:2)   TO  WRK-DATA-ATU(5:2).
           MOVE  WRK-TIMESTAMP-ATUAL(9:2)   TO  WRK-DATA-ATU(7:2).

      *----------------------------------------------------------------*
       11000-99-FIM.                   EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       20000-CONSISTIR-ENTRADA         SECTION.
      *----------------------------------------------------------------*

4S2511*    IF  ROTENT-CCNPJ-CPF        NOT NUMERIC OR
4S2511*        ROTENT-CCNPJ-CPF        EQUAL   ZEROS
4S2511     IF  ROTENT-CCNPJ-CPF        EQUAL   SPACES OR
4S2511         ROTENT-CCNPJ-CPF        EQUAL   LOW-VALUES
               MOVE '0223'             TO ROTSAI-COD-RETORNO
               MOVE SG-MENSAGEM(223)
                                       TO ROTSAI-MSG-RETORNO
               PERFORM 80000-FINALIZAR
           END-IF.

4S2511*    IF  ROTENT-CFLIAL-CNPJ      NOT NUMERIC
4S2511     IF  ROTENT-CFLIAL-CNPJ      EQUAL '0000' OR
4S2511         ROTENT-CFLIAL-CNPJ      EQUAL SPACES OR
4S2511         ROTENT-CFLIAL-CNPJ      EQUAL LOW-VALUES
               MOVE '0223'             TO ROTSAI-COD-RETORNO
               MOVE SG-MENSAGEM(223)
                                       TO ROTSAI-MSG-RETORNO
               PERFORM 80000-FINALIZAR
           END-IF.

           IF  ROTENT-CCTRL-CNPJ-CPF   NOT NUMERIC
               MOVE '0223'             TO ROTSAI-COD-RETORNO
               MOVE SG-MENSAGEM(223)
                                       TO ROTSAI-MSG-RETORNO
               PERFORM 80000-FINALIZAR
           END-IF.

           IF  ROTENT-DANO-OPER-DESC(1)    NOT NUMERIC OR
               ROTENT-DANO-OPER-DESC(1)    EQUAL   ZEROS
               MOVE  1                 TO ROTSAI-CAMPOS-ERRO(1)
               MOVE '0117'             TO ROTSAI-COD-RETORNO
               MOVE SG-MENSAGEM(117)
                                       TO ROTSAI-MSG-RETORNO
               PERFORM 80000-FINALIZAR
           END-IF.

           PERFORM  21000-VALIDAR-SITU-OPER
                    VARYING IND-ENT FROM 1 BY 1
                    UNTIL IND-ENT GREATER 70
                    OR  ROTENT-DANO-OPER-DESC(IND-ENT) = ZEROS.

      *----------------------------------------------------------------*
       20000-99-FIM.                   EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       21000-VALIDAR-SITU-OPER         SECTION.
      *----------------------------------------------------------------*

           MOVE ROTENT-DANO-OPER-DESC(IND-ENT)  TO  G4-DANO-OPER-DESC
           MOVE ROTENT-NSEQ-OPER-DESC(IND-ENT)  TO  G4-NSEQ-OPER-DESC
           MOVE 'TOTAL'                TO  G4-INSTRUCAO
           MOVE 'DCOM7320'             TO  WRK-MODULO

           CALL  WRK-MODULO            USING G4-ARGUMENTOS-ENTRADA
                                             G4-COD-RETORNO
                                             ERRO-AREA
                                             LKG-SQLCA.

           IF  G4-COD-RETORNO          NOT EQUAL ZEROS
               IF  G4-COD-RETORNO      EQUAL 03
                   MOVE '0102'             TO ROTSAI-COD-RETORNO
                   MOVE SG-MENSAGEM(0102)  TO ROTSAI-MSG-RETORNO
                   PERFORM 80000-FINALIZAR
               ELSE
                   MOVE G4-COD-RETORNO     TO ROTSAI-COD-RETORNO
                   MOVE G4-MENSAGEM        TO ROTSAI-MSG-RETORNO
                   PERFORM 80000-FINALIZAR
               END-IF
           END-IF.

           IF  G4RT-CSIT-DESC-COML(1)  NOT EQUAL 2 AND 3
               MOVE '1100'             TO ROTSAI-COD-RETORNO
               MOVE SG-MENSAGEM(1100)  TO ROTSAI-MSG-RETORNO
               PERFORM 80000-FINALIZAR
           END-IF.

      *----------------------------------------------------------------*
       21000-99-FIM.                   EXIT.
      *----------------------------------------------------------------*
           EJECT
      *----------------------------------------------------------------*
       30000-PROCESSAR                 SECTION.
      *----------------------------------------------------------------*

           PERFORM VARYING IND-ENT FROM 1 BY 1
                   UNTIL IND-ENT GREATER 70
               OR  ROTENT-DANO-OPER-DESC(IND-ENT) = ZEROS
                   PERFORM  32000-SELECIONA-ISENCAO
                   PERFORM  33000-TRATAR-EXCLUSAO
           END-PERFORM

           MOVE 'EXC' TO WRK-ENC-EXC

           MOVE 'S'       TO WRK-TEM-OPERACAO.
           PERFORM        40000-VALIDAR-EXCLUSAO-PESSOA.

           IF  WRK-TEM-OPERACAO    EQUAL 'N'
               PERFORM  41000-EXCLUIR-PESSOA
           END-IF.

           MOVE  'E'  TO  X1-CINDCD-MANUT-REG.
           PERFORM 51000-GRAVAR-LOG-OPER.

      *----------------------------------------------------------------*
       30000-99-FIM.                   EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       32000-SELECIONA-ISENCAO         SECTION.
      *----------------------------------------------------------------*

4S2511*    MOVE  ROTENT-CCNPJ-CPF
4S2511*                                TO  36-CCNPJ-CPF
4S2511     MOVE  ROTENT-CCNPJ-CPF      TO  36-CCNPJ-CPF-ST
           MOVE  ROTENT-DANO-OPER-DESC    (IND-ENT)
                                       TO  36-DANO-OPER-DESC.
           MOVE  ROTENT-NSEQ-OPER-DESC    (IND-ENT)
                                       TO  36-NSEQ-OPER-DESC.

           MOVE  'PARCIAL-04'          TO  36-INSTRUCAO.

           MOVE 'DCOM6680'             TO WRK-MODULO

           CALL  WRK-MODULO USING 36-ARGUMENTOS-ENTRADA
                                  36-RETORNO
                                  LKG-ERRO-AREA
                                  LKG-SQLCA.


           IF  36-COD-RETORNO NOT EQUAL ZEROS
               MOVE 36-COD-RETORNO TO ROTSAI-COD-RETORNO
               MOVE 36-MENSAGEM    TO ROTSAI-MSG-RETORNO
               PERFORM 80000-FINALIZAR
           END-IF.

           PERFORM 32100-VERIFICA-SITUACAO.

           MOVE LENGTH OF 36-OCORRENCIAS(01)
                                         TO X1-WREG-LOG-ANTES-LEN.
           MOVE 36-OCORRENCIAS(01)       TO X1-WREG-LOG-ANTES-TEXT.

      *----------------------------------------------------------------*
       32000-99-FIM.                  EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       32100-VERIFICA-SITUACAO         SECTION.
      *----------------------------------------------------------------*

           MOVE  ROTENT-DANO-OPER-DESC    (IND-ENT)
                                       TO  G4-DANO-OPER-DESC.
           MOVE  ROTENT-NSEQ-OPER-DESC    (IND-ENT)
                                       TO  G4-NSEQ-OPER-DESC.
           MOVE  004                   TO  G4-CSIT-DESC-COML.

           MOVE  'PARCIAL-02'          TO  G4-INSTRUCAO.

           MOVE 'DCOM7320'             TO WRK-MODULO

           CALL  WRK-MODULO USING G4-ARGUMENTOS-ENTRADA
                                  G4-RETORNO
                                  LKG-ERRO-AREA
                                  LKG-SQLCA.

           IF  G4-COD-RETORNO  EQUAL ZEROS
               MOVE '1113'                TO ROTSAI-COD-RETORNO
               MOVE SG-MENSAGEM(1113)
                                          TO ROTSAI-MSG-RETORNO
               PERFORM 80000-FINALIZAR
           END-IF.

      *----------------------------------------------------------------*
       32100-99-FIM.                  EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       33000-TRATAR-EXCLUSAO           SECTION.
      *----------------------------------------------------------------*

           MOVE  ROTENT-DANO-OPER-DESC    (IND-ENT)
                                       TO  36-DANO-OPER-DESC.
           MOVE  ROTENT-NSEQ-OPER-DESC    (IND-ENT)
                                       TO  36-NSEQ-OPER-DESC.

           MOVE  'PARCIAL-01'          TO  36-INSTRUCAO.

           MOVE  'DCOM6684'            TO  WRK-MODULO.

           CALL  WRK-MODULO         USING  36-ARGUMENTOS-ENTRADA
                                           36-RETORNO
                                           LKG-ERRO-AREA
                                           LKG-SQLCA.

           IF  36-COD-RETORNO  NOT EQUAL ZEROS AND 2 AND 3
               MOVE '0099'                TO ROTSAI-COD-RETORNO
               MOVE SG-MENSAGEM(677)
                                          TO ROTSAI-MSG-RETORNO
               PERFORM 80000-FINALIZAR
           END-IF.

           MOVE  ROTENT-DANO-OPER-DESC(IND-ENT) TO G9-DANO-OPER-DESC.
           MOVE  ROTENT-NSEQ-OPER-DESC(IND-ENT) TO G9-NSEQ-OPER-DESC.
           MOVE  37                             TO G9-CEVNTO-DESC-COML.
           MOVE  00                             TO G9-CSIT-INIC-DESC.
           MOVE  21                             TO G9-CSIT-FNAL-DESC.
           MOVE  WRK-TIMESTAMP-ATUAL            TO G9-HSIT-INIC-DESC
                                                   G9-HSIT-FNAL-DESC.

           MOVE  'PARCIAL-02'                   TO G9-INSTRUCAO.
           MOVE  'DCOM7348'                     TO WRK-MODULO.

           CALL      WRK-MODULO         USING   G9-ARGUMENTOS-ENTRADA
                                                G9-RETORNO
                                                LKG-ERRO-AREA
                                                LKG-SQLCA.

           IF  G9-COD-RETORNO NOT EQUAL ZEROS
               IF  G9-COD-RETORNO EQUAL 4
                   MOVE '0100'          TO ROTSAI-COD-RETORNO
                   MOVE SG-MENSAGEM(100)
                                        TO  ROTSAI-MSG-RETORNO
               ELSE
                   MOVE '0099'          TO ROTSAI-COD-RETORNO
                   MOVE SG-MENSAGEM(658)
                                        TO  ROTSAI-MSG-RETORNO
               END-IF
               PERFORM 80000-FINALIZAR
           END-IF.

           MOVE  ROTENT-DANO-OPER-DESC(IND-ENT) TO G9-DANO-OPER-DESC.
           MOVE  ROTENT-NSEQ-OPER-DESC(IND-ENT) TO G9-NSEQ-OPER-DESC.
           MOVE  35                             TO G9-CEVNTO-DESC-COML.
           MOVE  22                             TO G9-CSIT-INIC-DESC.
           MOVE  21                             TO G9-CSIT-FNAL-DESC.
           MOVE  WRK-TIMESTAMP-ATUAL            TO G9-HSIT-INIC-DESC
                                                   G9-HSIT-FNAL-DESC.

           MOVE  'PARCIAL-02'                   TO G9-INSTRUCAO.
           MOVE  'DCOM7348'                     TO WRK-MODULO.

           CALL      WRK-MODULO         USING   G9-ARGUMENTOS-ENTRADA
                                                G9-RETORNO
                                                LKG-ERRO-AREA
                                                LKG-SQLCA.

           IF  G9-COD-RETORNO NOT EQUAL ZEROS
               IF  G9-COD-RETORNO EQUAL 4
                   MOVE '0100'          TO ROTSAI-COD-RETORNO
                   MOVE SG-MENSAGEM(100)
                                        TO  ROTSAI-MSG-RETORNO
               ELSE
                   MOVE '0099'          TO ROTSAI-COD-RETORNO
                   MOVE SG-MENSAGEM(658)
                                        TO  ROTSAI-MSG-RETORNO
               END-IF
               PERFORM 80000-FINALIZAR
           END-IF.

      *----------------------------------------------------------------*
       33000-99-FIM.                   EXIT.
      *----------------------------------------------------------------*

      *---------------------------------------------------------------
       40000-VALIDAR-EXCLUSAO-PESSOA    SECTION.
      *---------------------------------------------------------------

           MOVE  'PARCIAL-11'                TO 36-INSTRUCAO.
4S2511*    MOVE  ROTENT-CCNPJ-CPF            TO 36-CCNPJ-CPF.
4S2511     MOVE  ROTENT-CCNPJ-CPF            TO 36-CCNPJ-CPF-ST.
           MOVE  001                         TO 36-CTPO-ISENC-DESC.
4S2511*    MOVE  ROTENT-CFLIAL-CNPJ          TO 36-CFLIAL-CNPJ.
4S2511     MOVE  ROTENT-CFLIAL-CNPJ          TO 36-CFLIAL-CNPJ-ST.
           MOVE  ROTENT-CCTRL-CNPJ-CPF       TO 36-CCTRL-CNPJ-CPF.
4S2511     MOVE  ROTENT-CCTRL-CNPJ-CPF       TO 36-CCTRL-CNPJ-CPF-ST.

           MOVE  'DCOM6681'                  TO WRK-MODULO.

           CALL  WRK-MODULO                    USING
                                               36-ARGUMENTOS-ENTRADA
                                               36-RETORNO
                                               LKG-ERRO-AREA
                                               LKG-SQLCA.

           IF  36-COD-RETORNO NOT EQUAL ZEROS AND 2 AND 3
               MOVE '0099'                TO ROTSAI-COD-RETORNO
               MOVE SG-MENSAGEM(678)
                                          TO ROTSAI-MSG-RETORNO
               PERFORM 80000-FINALIZAR
           ELSE
               IF  36-QTDE-TOTAL = ZEROS
                   MOVE 'N' TO WRK-TEM-OPERACAO
               END-IF
           END-IF.

      *---------------------------------------------------------------
       40000-FIM.
      *---------------------------------------------------------------

      *---------------------------------------------------------------
       41000-EXCLUIR-PESSOA    SECTION.
      *---------------------------------------------------------------

4S2511*    MOVE  ROTENT-CCNPJ-CPF            TO 48-CCNPJ-CPF.
4S2511     MOVE  ROTENT-CCNPJ-CPF            TO 48-CCNPJ-CPF-ST.
           MOVE  001                         TO 48-CTPO-ISENC-DESC.
4S2511*    MOVE  ROTENT-CFLIAL-CNPJ          TO 48-CFLIAL-CNPJ.
4S2511     MOVE  ROTENT-CFLIAL-CNPJ          TO 48-CFLIAL-CNPJ-ST.
           MOVE  ROTENT-CCTRL-CNPJ-CPF       TO 48-CCTRL-CNPJ-CPF.
4S2511     MOVE  ROTENT-CCTRL-CNPJ-CPF       TO 48-CCTRL-CNPJ-CPF-ST.

           MOVE  'PARCIAL-01'                TO 48-INSTRUCAO.

           MOVE  'DCOM6744'                  TO WRK-MODULO.

           CALL  WRK-MODULO                    USING
                                               48-ARGUMENTOS-ENTRADA
                                               48-RETORNO
                                               LKG-ERRO-AREA
                                               LKG-SQLCA.

           IF  48-COD-RETORNO NOT EQUAL ZEROS AND 2
               IF  48-COD-RETORNO EQUAL 3
                   MOVE '0548'                TO ROTSAI-COD-RETORNO
                   MOVE SG-MENSAGEM(548)
                                              TO ROTSAI-MSG-RETORNO
               ELSE
                   MOVE '0099'                TO ROTSAI-COD-RETORNO
                   MOVE SG-MENSAGEM(677)
                                              TO ROTSAI-MSG-RETORNO
               END-IF
               PERFORM 80000-FINALIZAR
           END-IF.

      *---------------------------------------------------------------
       41000-FIM.
      *---------------------------------------------------------------

      *----------------------------------------------------------------*
       51000-GRAVAR-LOG-OPER           SECTION.
      *----------------------------------------------------------------*

           MOVE  002                   TO  X1-CELMTO-DESC-COML.
           MOVE  057                   TO  X1-CEVNTO-LOG-DESC.

           MOVE  ROTENT-CCNPJ-CPF      TO  X1-CCHAVE-ELMTO-DESC (01:09).
           MOVE  ROTENT-CFLIAL-CNPJ    TO  X1-CCHAVE-ELMTO-DESC (10:04).
           MOVE  ROTENT-CCTRL-CNPJ-CPF TO  X1-CCHAVE-ELMTO-DESC (14:02).

           MOVE  ZEROS                 TO  X1-WREG-LOG-APOS-LEN
           MOVE  SPACES                TO  X1-WREG-LOG-APOS-TEXT

           MOVE  LENGTH OF 36-OCORRENCIAS(1)
                                       TO  X1-WREG-LOG-ANTES-LEN.
           MOVE  36-OCORRENCIAS(1)     TO  X1-WREG-LOG-ANTES-TEXT.
           MOVE  'DCOM5058'            TO  X1-CTRANS-PROG-ATULZ.
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
               MOVE '0099'         TO  ROTSAI-COD-RETORNO
               MOVE SG-MENSAGEM(578)
                                   TO  ROTSAI-MSG-RETORNO
               PERFORM  80000-FINALIZAR
           END-IF.

      *----------------------------------------------------------------*
       51000-99-FIM.                   EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       80000-FINALIZAR                 SECTION.
      *----------------------------------------------------------------*

           IF  ROTSAI-COD-RETORNO NOT EQUAL '0000' AND '0002' AND
                                            '0003' AND '0004' AND
                                            '0005'
               NEXT SENTENCE
           ELSE
               MOVE  'DCOM5058'    TO  ERR-PGM
               MOVE  SPACES        TO  LKG-SQLCA
               MOVE  ERRO-AREA     TO  LKG-ERRO-AREA
           END-IF.

           GOBACK.

      *----------------------------------------------------------------*
       80000-FIM. EXIT.
      *----------------------------------------------------------------*
