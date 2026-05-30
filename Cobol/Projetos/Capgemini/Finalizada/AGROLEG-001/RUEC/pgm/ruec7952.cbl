      *================================================================*
       IDENTIFICATION                  DIVISION.
      *================================================================*

       PROGRAM-ID.     RUEC7952.
       AUTHOR.         VINICIUS MEIRELLES.
      *================================================================*
      *                         7 C O M M                              *
      *----------------------------------------------------------------*
      *     PROGRAMA....: RUEC7952                                     *
      *     PROGRAMADOR.: VINICIUS MEIRELLES                           *
      *     ANALISTA....: VINICIUS MEIRELLES                           *
      *     DATA........: 01/2026                                      *
      *----------------------------------------------------------------*
      *     OBJETIVO....: INCLUSAO DE PARCELAS PARA RENEGOCIACAO       *
      *----------------------------------------------------------------*
      *     TELAS.......:                                              *
      *     RUECT798....: INCLUSAO DE PARCELAS PARA RENEGOCIACAO       *
      *----------------------------------------------------------------*
      *     INC'S.......:                                              *
      *     I#BRAD7C - AREA DE ERRO DA BRAD7100                        *
      *----------------------------------------------------------------*
      *     MODULOS.....:                                              *
      *     BRAD0660 - INSERIR CARACTERES DE NULL NA MENSAGEM DE SAIDA *
      *     BRAD7100 - TRATAMENTO DE ERROS QUANDO PROGRAMA INVALIDO    *
      *     POOL5005 - CONTROLE DO TRANSITO DE MENSAGENS NO IMS/DC     *
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
       DATA                             DIVISION.
      *================================================================*

      *----------------------------------------------------------------*
       WORKING-STORAGE                 SECTION.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(032)         VALUE
           '** RUEC7952 - AREA DE WORKING **'.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(032)         VALUE
           '*** VARIAVEIS AUXILIARES     ***'.
      *----------------------------------------------------------------*
       01  WRK-INCONSIS                PIC  X(001)         VALUE SPACES.
       01  WRK-FIM-SEL                 PIC  X(001)         VALUE SPACES.
       01  WRK-INDICE                  PIC  X(001)         VALUE SPACES.
       01  WRK-VRENEG-CONTR-RURAL      PIC S9(015)V99
                                                   COMP-3  VALUE ZEROS.

       01  WRK-S9-15V2                 PIC +9(15)V99       VALUE ZEROS.
       01  FILLER REDEFINES WRK-S9-15V2.
           05 FILLER                   PIC  X(01).
           05 FILLER                   PIC  X(07).
           05 WRK-9-08V2               PIC  9(08)V99.
       01  FILLER REDEFINES WRK-S9-15V2.
           05 FILLER                   PIC  X(01).
           05 FILLER                   PIC  X(06).
           05 WRK-9-09V2               PIC  9(09)V99.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(032)         VALUE
           '*** VARIAVEIS PAGINACAO      ***'.
      *----------------------------------------------------------------*
       01  WRK-PAGINACAO.
           05  WRK-TOTAL-REG           PIC  9(03)  COMP-3  VALUE ZEROS.
           05  WRK-PAG-ATUAL           PIC  9(03)  COMP-3  VALUE ZEROS.
           05  WRK-IND-PRIMEIRO-REG    PIC  9(03)  COMP-3  VALUE ZEROS.
           05  WRK-FIM-TEORICO         PIC  9(03)  COMP-3  VALUE ZEROS.
           05  WRK-IND-P-MAX           PIC  9(003) COMP-3  VALUE ZEROS.
           05  IND-1                   PIC  9(003) COMP-3  VALUE ZEROS.
           05  IND-2                   PIC  9(003) COMP-3  VALUE ZEROS.
           05  IND-3                   PIC  9(003) COMP-3  VALUE ZEROS.
           05  IND-4                   PIC  9(003) COMP-3  VALUE ZEROS.
           05  IND-5                   PIC  9(003) COMP-3  VALUE ZEROS.
           05  WRK-RESTO               PIC  9(009) COMP-3  VALUE ZEROS.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(032)         VALUE
           '*** TABELA INTERNA           ***'.
      *----------------------------------------------------------------*
       01 WRK-TAB-DADOS-INT.
          03 WRK-TAB-DADOS            OCCURS 120.
             05 WRK-TAB-NPCELA        PIC S9(003)V  COMP-3 VALUE ZEROS.
             05 WRK-TAB-CINDPC        PIC  X(001)        VALUE SPACES.
             05 WRK-TAB-DTVENC        PIC  X(010)        VALUE SPACES.
             05 WRK-TAB-SLDPRI        PIC S9(15)V99 COMP-3 VALUE ZEROS.
             05 WRK-TAB-SLDENC        PIC S9(15)V99 COMP-3 VALUE ZEROS.
             05 WRK-TAB-SLDTOT        PIC S9(15)V99 COMP-3 VALUE ZEROS.
      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(032)         VALUE
           '*** AREA DA POOL5000         ***'.
      *----------------------------------------------------------------*

       01  WRK-GU                      PIC  X(004)         VALUE 'GU'.
       01  WRK-CHNG                    PIC  X(004)         VALUE 'CHNG'.
       01  WRK-ISRT                    PIC  X(004)         VALUE 'ISRT'.
       01  WRK-FUNCAO                  PIC  X(004)         VALUE SPACES.
       01  WRK-TELA                    PIC  X(008)         VALUE SPACES.
       01  WRK-NOVA-OPCAO              PIC  9(002)         VALUE ZEROS.
       01  WRK-OPCAO-ATUAL             PIC  9(002)         VALUE ZEROS.

       01  WRK-COD-USER-X.
           05 WRK-COD-USER             PIC  9(007)         VALUE ZEROS.

       01  WRK-COD-DEPTO.
           05 FILLER                   PIC  X(002)         VALUE SPACES.
           05 WRK-COD-DEPTO-N          PIC  9(004)         VALUE ZEROS.

       01  WRK-MENSAGEM.
           05 FILLER                   PIC  X(004)         VALUE SPACES.
           05 WRK-TRANSACAO            PIC  X(008)         VALUE SPACES.
           05 FILLER                   PIC  X(1988)        VALUE SPACES.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(032)         VALUE
           '*** AREA DE ERRO DA BRAD0660 ***'.
      *----------------------------------------------------------------*

       01  WRK-ERRO-BRAD0660.
           05  FILLER                  PIC  X(045)       VALUE
               '* RETURN-CODE DIFERENTE DE ZEROS NA BRAD0660'.
           05  FILLER                  PIC  X(012)       VALUE
               '- RET.CODE ='.
           05  WRK-RETURN-CODE         PIC  9(002)       VALUE ZEROS.
           05  FILLER                  PIC  X(011)       VALUE
               ' - LOCAL ='.
           05  WRK-LOCAL-ERRO          PIC  9(002)       VALUE ZEROS.
           05  FILLER                  PIC  X(002)       VALUE ' *'.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(032)         VALUE
           '*** AREA DA RUECT798 - INPUT ***'.
      *----------------------------------------------------------------*

       01  WRK-INPUT.
           03 FILLER                   PIC  X(018)         VALUE SPACES.
           03 WRK-I-PFK                PIC  X(001)         VALUE SPACES.
           03 WRK-I-SENHAS             PIC  X(037)         VALUE SPACES.
           03 WRK-I-PAGINA             PIC  9(004)         VALUE ZEROS.
           03 WRK-I-FIMPAG             PIC  X(001)         VALUE SPACES.
           03 WRK-I-CTERM              PIC  X(008)         VALUE SPACES.
           03 WRK-I-FASE               PIC  X(001)         VALUE SPACES.
           03 WRK-I-CHVOPE             PIC  9(009)         VALUE ZEROS.
           03 WRK-I-REFBAC             PIC  X(011)         VALUE ZEROS.
           03 WRK-I-DADOS              OCCURS 012.
              05 WRK-I-CINDPC          PIC  X(001)         VALUE SPACES.
              05 WRK-I-DTVENC          PIC  X(010)         VALUE SPACES.
              05 WRK-I-SLDPRI.
                 07 WRK-I-SLDPRI-N     PIC  ZZ.ZZZ.ZZ9,99   VALUE ZEROS.
              05 WRK-I-SLDENC.
                 07 WRK-I-SLDENC-N     PIC  ZZ.ZZZ.ZZ9,99   VALUE ZEROS.
              05 WRK-I-SLDTOT.
                 07 WRK-I-SLDTOT-N     PIC ZZZ.ZZZ.ZZ9,99  VALUE ZEROS.
           03 WRK-I-MENSA              PIC  X(79)          VALUE SPACES.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(032)         VALUE
           '*** AREA RUECT798  - OUTPUT  ***'.
      *----------------------------------------------------------------*

       01  WRK-OUTPUT.
           05 WRK-O-LL                 PIC S9(004) COMP    VALUE ZEROS.
           05 WRK-O-ZZ                 PIC S9(004) COMP    VALUE ZEROS.
           03 WRK-O-SENHAS             PIC  X(037)         VALUE SPACES.
           03 WRK-O-PAGINA             PIC  9(004)         VALUE ZEROS.
           03 WRK-O-FIMPAG             PIC  X(001)         VALUE SPACES.
           03 WRK-O-CTERM              PIC  X(008)         VALUE SPACES.
           03 WRK-O-FASE               PIC  X(001)         VALUE SPACES.
           03 WRK-O-CHVOPE             PIC  9(009)         VALUE ZEROS.
           03 WRK-O-REFBAC             PIC  X(011)         VALUE ZEROS.
           03 WRK-O-DADOS              OCCURS 012.
              05 WRK-O-CINDPC-ATTR     PIC S9(004) COMP    VALUE ZEROS.
              05 WRK-O-CINDPC          PIC  X(001)         VALUE SPACES.
              05 WRK-O-DTVENC          PIC  X(010)         VALUE SPACES.
              05 WRK-O-SLDPRI.
                 07 WRK-O-SLDPRI-N     PIC  ZZ.ZZZ.ZZ9,99   VALUE ZEROS.
              05 WRK-O-SLDENC.
                 07 WRK-O-SLDENC-N     PIC  ZZ.ZZZ.ZZ9,99   VALUE ZEROS.
              05 WRK-O-SLDTOT.
                 07 WRK-O-SLDTOT-N     PIC ZZZ.ZZZ.ZZ9,99  VALUE ZEROS.
           03 WRK-O-MENSA              PIC  X(79)          VALUE SPACES.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(032)         VALUE
           '*** AREA RUECT798 - BRAD0660 ***'.
      *----------------------------------------------------------------*

       01  WRK-660-RUECT798.
           03 WRK-660-LL                 PIC S9(004) COMP  VALUE ZEROS.
           03 WRK-660-LL-OUTPUT          PIC S9(004) COMP  VALUE ZEROS.
           03 WRK-660-SENHAS             PIC S9(004) COMP  VALUE 0037.
           03 WRK-660-PAGINA             PIC S9(004) COMP  VALUE 0004.
           03 WRK-660-FIMPAG             PIC S9(004) COMP  VALUE 0001.
           03 WRK-660-CTERM              PIC S9(004) COMP  VALUE 0008.
           03 WRK-660-FASE               PIC S9(004) COMP  VALUE 0001.
           03 WRK-660-CHVOPE             PIC S9(004) COMP  VALUE 0009.
           03 WRK-660-REFBAC             PIC S9(004) COMP  VALUE 0011.
           03 WRK-660-LISTA              OCCURS 12.
              05 WRK-660-CINDPC          PIC S9(004) COMP  VALUE 2003.
              05 WRK-660-DTVENC          PIC S9(004) COMP  VALUE 0010.
              05 WRK-660-SLDPRI          PIC S9(004) COMP  VALUE 0013.
              05 WRK-660-SLDENC          PIC S9(004) COMP  VALUE 0013.
              05 WRK-660-SLDTOT          PIC S9(004) COMP  VALUE 0014.
           03 WRK-660-MENSA              PIC S9(004) COMP  VALUE 0079.

      *---------------------------------------------------------------*
      *                    DEFINICAO DE ATRIBUTOS                     *
      *---------------------------------------------------------------*

       01  FILLER.
           03  TAB-ATRIBUTOS.

      *****************************************************************
      *    BRILHANTE, DESPROT, COM POS. CURSOR, NUM                   *
               05  WRK-49369           PIC S9(008) COMP VALUE +49369.
      *****************************************************************
      *    NORMAL, DESPROT, COM POS. CURSOR, NUM                      *
               05  WRK-49361           PIC S9(008) COMP VALUE +49361.
      *****************************************************************
      *    PROTEGIDO (NUMERICO OU ALFA)                               *
               05  WRK-225             PIC S9(008) COMP VALUE +225.
      *****************************************************************
      *    BRILHANTE, DESPROT, COM POS. CURSOR, ALFA                  *
               05  WRK-49353           PIC S9(008) COMP VALUE +49353.
      *****************************************************************
      *    PROTEGIDO, BRILHANTE (NUMERICO OU ALFA)                    *
               05  WRK-169             PIC S9(008) COMP VALUE +169.
      *****************************************************************
      *    PROTEGIDO, BRILHANTE, CURSOR (NUMERICO OU ALFA)            *
               05  WRK-49385           PIC S9(008) COMP VALUE +49385.
      *****************************************************************
      *    NORMAL, DESPROT, CURSOR, ALFA                              *
               05  WRK-49345           PIC S9(008) COMP VALUE +49345.
      *****************************************************************
      *    NORMAL, DESPROT, NUM                                       *
               05  WRK-209             PIC S9(008) COMP VALUE +209.
      *****************************************************************
      *    NORMAL, DESPROT, ALFA                                      *
               05  WRK-193             PIC S9(008) COMP VALUE +193.
      *****************************************************************

           03  FILLER REDEFINES TAB-ATRIBUTOS OCCURS 9 TIMES
                                INDEXED BY IND-ATRIB.
               05  TAB-ATRIBUTO        PIC S9(008) COMP.

      *---------------------------------------------------------------*
      *                    TABELA DE MENSAGENS                        *
      *---------------------------------------------------------------*

       01  WRK-MENS01                  PIC X(79) VALUE
           'MARQUE/DESMARQUE COM ''X'' A(S) PARCELA(S) E TECLE PF6'.

       01  WRK-MENS02                  PIC X(79) VALUE
           'MARQUE COM ''X'' OU DESMARQUE A PARCELA'.

       01  WRK-MENS03                  PIC X(79) VALUE
           'TECLE PF6 PARA SALVAR MARCACAO DE PARCELAS'.

       01  WRK-MENS04                  PIC X(79) VALUE
           'PF INVALIDA'.

       01  WRK-MENS05                  PIC X(79) VALUE
           'REGISTRO(S) ATUALIZADO(S)'.

       01  WRK-MENS06                  PIC X(79) VALUE
           'NAO EXISTE MAIS PAGINAS PARA VOLTAR'.

       01  WRK-MENS07                  PIC X(79) VALUE
           'NAO EXISTE MAIS PAGINAS PARA AVANCAR'.

       01  WRK-MENS08                  PIC X(79) VALUE
           'NENHUM REGISTRO ENCOTRADO NA TABELA DE PARCELAS'.

       01  WRK-MENS09                  PIC X(79) VALUE
           'REMOVER VALOR INCORRETO EM NA(S) PARCELA(S) - DIF ''X'''.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(032)         VALUE
           '* COPY I#RUEC00 - NAVEG-RURAL  *'.
      *----------------------------------------------------------------*

       COPY 'I#RUEC00'.

       01  FILLER                      PIC  X(032)         VALUE
           '*** AREA DE COMUNICACAO      ***'.
      *----------------------------------------------------------------*

       01 WRK-INPUT-7951.
          05 WRK-INP-7951-CJUNC         PIC  9(05)     VALUE ZEROS.
          05 WRK-INP-7951-CEDULA        PIC  9(09)     VALUE ZEROS.
          05 WRK-INP-7951-CREFBACEN     PIC  X(11)     VALUE SPACES.
          05 WRK-INP-7951-CHVOPE        PIC  9(09)     VALUE ZEROS.

       01 WRK-COMU-7951-AREA.
          03 WRK-COMU-7951-LL           PIC S9(004) COMP
                                                       VALUE ZEROS.
          03 WRK-COMU-7951-ZZ           PIC S9(004) COMP
                                                       VALUE ZEROS.
          03 WRK-COMU-7951-TRANCODE.
             10 WRK-COMU-7951-TRAN      PIC  X(008)    VALUE SPACES.
             10 FILLER                  PIC  X(006)    VALUE SPACES.
             10 WRK-COMU-7951-PFK       PIC  X(001)    VALUE SPACES.
          03 WRK-COMU-7951-SENHAS       PIC  X(037)    VALUE SPACES.
          03 WRK-COMU-7951-OPCAO        PIC  9(002)    VALUE ZEROS.
          03 WRK-COMU-7951-TERM         PIC  X(008)    VALUE SPACES.
          03 WRK-COMU-7951-STATUS       PIC  X(002)    VALUE SPACES.
          03 WRK-COMU-7951-MENSAGEM     PIC  X(079)    VALUE SPACES.
          03 WRK-COMU-7951-PROX-TELA    PIC  X(008)    VALUE SPACES.
          03 FILLER                     PIC S9(15)V9(02)
                                                       VALUE ZEROS.
       01  WRK-COMU-AREA.
           05  WRK-COMU-LL             PIC S9(004) COMP  VALUE ZEROS.
           05  WRK-COMU-ZZ             PIC S9(004) COMP  VALUE ZEROS.
           05  WRK-COMU-TRANCODE.
               10  WRK-COMU-TRANSACAO  PIC  X(008)       VALUE SPACES.
               10  FILLER              PIC  X(006)       VALUE SPACES.
               10  WRK-COMU-PFK        PIC  X(001)       VALUE SPACES.
           05  WRK-COMU-SENHAS         PIC  X(037)       VALUE SPACES.
           05  WRK-COMU-OPCAO          PIC  9(002)       VALUE ZEROS.
           05  WRK-COMU-CTERM          PIC  X(008)       VALUE SPACES.
           05  WRK-COMU-STATUS         PIC  X(002)       VALUE SPACES.
           05  WRK-COMU-MENSAGEM       PIC  X(079)       VALUE SPACES.
           05  WRK-COMU-ANTE-TELA      PIC  X(008)       VALUE SPACES.
           05  WRK-COMU-NAVEGACAO      PIC  X(300)       VALUE SPACES.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(032)         VALUE
           '*** AREA DA BRAD7100         ***'.
      *----------------------------------------------------------------*

       COPY 'I#BRAD7C'.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(032)         VALUE
           '*** AREA PARA DB2            ***'.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(032)         VALUE
           '*** INCLUDE SQLCA            ***'.
      *----------------------------------------------------------------*

           EXEC SQL
               INCLUDE SQLCA
           END-EXEC.

           EXEC SQL
               INCLUDE RUECB042
           END-EXEC.

           EXEC SQL
               INCLUDE RUECV007
           END-EXEC.

           EXEC SQL
               INCLUDE RUECB00C
           END-EXEC.

           EXEC SQL
               INCLUDE RUECB00D
           END-EXEC.

      *----------------------------------------------------------------*
       01  FILLER                      PIC X(50)           VALUE
           '*  DEFINICAO CURSOR CSR01-RUECB00D    *'.
      *----------------------------------------------------------------*

           EXEC SQL
           DECLARE CSR01-RUECB00D CURSOR  WITH HOLD  FOR
             SELECT
               CREFT_RECOR
              ,CTPO_EVNTO_RURAL
              ,DEVNTO
              ,NPCELA_CONTR
              ,DVCTO
              ,VSDO_DVDOR_PRINC
              ,VTOT_ENCAR
              ,VTOT_JURO_MORA
              ,VTOT_MULTA
              ,CINDCD_PCEL_RENEG
             FROM  DB2PRD.TPCELA_TEMPR_RENEG
             WHERE CJUNC_DEPDC         =  :RUECB00D.CJUNC-DEPDC
               AND CCDULA              =  :RUECB00D.CCDULA
               AND CREFT_RECOR         =  :RUECB00D.CREFT-RECOR
             ORDER BY  DVCTO
           END-EXEC.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(032)         VALUE
           '** RUEC7952 - FIM DA WORKING  **'.
      *----------------------------------------------------------------*
      *----------------------------------------------------------------*
       LINKAGE                         SECTION.
      *----------------------------------------------------------------*
       01  LNK-IO-PCB.
           05  LNK-IO-LTERM            PIC  X(008).
           05  FILLER                  PIC  X(002).
           05  LNK-IO-STATUS           PIC  X(002).
           05  FILLER                  PIC  X(012).
           05  LNK-IO-MODNAME          PIC  X(008).

       01  LNK-ALT-PCB.
           05  LNK-ALT-LTERM           PIC  X(008).
           05  FILLER                  PIC  X(002).
           05  LNK-ALT-STATUS          PIC  X(002).
           05  FILLER                  PIC  X(012).
           05  LNK-ALT-MODNAME         PIC  X(008).

      *================================================================*
       PROCEDURE                       DIVISION.
      *================================================================*
      *----------------------------------------------------------------*
       0000-INICIAR                    SECTION.
      *----------------------------------------------------------------*

           ENTRY  'DLITCBL'            USING  LNK-IO-PCB
                                              LNK-ALT-PCB.


           MOVE  WRK-GU                TO  WRK-FUNCAO.

      *----------------------------------------------------------------*
       0000-10-POOL5000.
      *----------------------------------------------------------------*

           CALL 'POOL5000'             USING WRK-FUNCAO
                                             LNK-IO-PCB
                                             LNK-ALT-PCB
                                             WRK-MENSAGEM
                                             WRK-TELA
                                             WRK-NOVA-OPCAO
                                             WRK-COD-USER
                                             WRK-OPCAO-ATUAL
                                             WRK-COD-DEPTO.

AQUI       DISPLAY '&&RUEC7952 RETURN-CODE   : ' RETURN-CODE
                                                 UPON CONSOLE

           IF  RETURN-CODE          EQUAL  04 OR 12
               PERFORM 8000-FINALIZAR
           END-IF.

           PERFORM 1000-PROCESSAR-MENSAGEM.

           GO TO 0000-10-POOL5000.

      *----------------------------------------------------------------*
       0000-99-FIM.                    EXIT.
      *----------------------------------------------------------------*
      *----------------------------------------------------------------*
       1000-PROCESSAR-MENSAGEM         SECTION.
      *----------------------------------------------------------------*

           MOVE  'RUECT798'            TO  WRK-TELA.

AQUI       DISPLAY '&&RUEC7952 WRK-TRANSACAO : ' WRK-TRANSACAO
                                                 UPON CONSOLE

           IF WRK-TRANSACAO            NOT EQUAL  'RUEC7952'
              PERFORM 1200-RECEBER-CONTROLE
           ELSE
              PERFORM 3000-PROCESSAR-RUECT798
           END-IF.

AQUI       DISPLAY '&&RUEC7952 WRK-TELA      : ' WRK-TELA
                                                 UPON CONSOLE

           IF WRK-TELA              EQUAL  'RUECT798'
              PERFORM 7000-PROCESSAR-BRAD0660
           END-IF.

      *----------------------------------------------------------------*
       1000-99-FIM.                    EXIT.
      *----------------------------------------------------------------*
      *----------------------------------------------------------------*
       1100-ACESSAR-RUECB042           SECTION.
      *----------------------------------------------------------------*

           MOVE WRK-I-CTERM            TO CTERM            OF RUECB042.

           EXEC SQL
             SELECT
               WNAVG_CREDT_RURAL
             INTO
               :RUECB042.WNAVG-CREDT-RURAL
             FROM  DB2PRD.NAVEGACAO_RURAL
             WHERE CTERM               =  :RUECB042.CTERM
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS )
           OR (SQLWARN0                EQUAL 'W'       )
           OR (SQLCODE                 EQUAL +100      )
               MOVE 'DB2'              TO ERR-TIPO-ACESSO
               MOVE 'RUECB042'         TO ERR-DBD-TAB
               MOVE SQLWARN            TO ERR-DBD-TAB (9:8)
               MOVE 'SELECT'           TO ERR-FUN-COMANDO
               MOVE SQLCODE            TO ERR-SQL-CODE
               MOVE '1100'             TO ERR-LOCAL
               MOVE SPACES             TO ERR-SEGM
               PERFORM 9999-ROTINA-ERRO
           END-IF.

           MOVE WNAVG-CREDT-RURAL-TEXT OF RUECB042
                                       TO NAVEG-RURAL.

      *----------------------------------------------------------------*
       1100-99-FIM.                    EXIT.
      *----------------------------------------------------------------*
      *----------------------------------------------------------------*
       1200-RECEBER-CONTROLE           SECTION.
      *----------------------------------------------------------------*

           MOVE WRK-MENSAGEM           TO WRK-COMU-AREA.

           MOVE WRK-COMU-NAVEGACAO     TO WRK-INPUT-7951

           MOVE WRK-COMU-SENHAS        TO WRK-I-SENHAS
                                          WRK-O-SENHAS.
           MOVE WRK-COMU-CTERM         TO WRK-I-CTERM
                                          WRK-O-CTERM.
           MOVE 01                     TO WRK-I-PAGINA
                                          WRK-O-PAGINA
                                          WRK-PAG-ATUAL.

AQUI       DISPLAY '&&RUEC7952 COMU-TRANCODE : ' WRK-COMU-TRANCODE
                                                 UPON CONSOLE
           DISPLAY '&&RUEC7952 COMU-SENHAS   : ' WRK-COMU-SENHAS
                                                 UPON CONSOLE
           DISPLAY '&&RUEC7952 COMU-CTERM    : ' WRK-COMU-CTERM
                                                 UPON CONSOLE
           DISPLAY '&&RUEC7952 INPUT-7951    : ' WRK-INPUT-7951
                                                 UPON CONSOLE

           PERFORM 1100-ACESSAR-RUECB042.

           MOVE 'RUEC7952'             TO PROGRAMA-ANT.
           MOVE NAVEG-RURAL            TO WNAVG-CREDT-RURAL-TEXT
                                          OF RUECB042
           PERFORM 1222-UPDATE-RUECB042.

           PERFORM 1215-MONTAR-TELA

           PERFORM 4000-CONSISTIR-AREA-ENTRADA

      **** CASO NAO TENHA ENCONTRADO REGISTROS DE PARCELAS ENVIA MSG
AQUI       DISPLAY '&&RUEC7952 WRK-TOTAL-REG : ' WRK-TOTAL-REG
                                                 UPON CONSOLE

           IF WRK-TOTAL-REG            EQUAL ZEROS
              MOVE WRK-MENS08             TO WRK-O-MENSA
           ELSE
              MOVE WRK-MENS01             TO WRK-O-MENSA
           END-IF.

      *----------------------------------------------------------------*
       1200-99-FIM.                    EXIT.
      *----------------------------------------------------------------*
      *----------------------------------------------------------------*
       1215-MONTAR-TELA                SECTION.
      *----------------------------------------------------------------*

           PERFORM 1216-LIMPAR-LISTA-SAIDA.

           PERFORM 1217-RECEBE-VALORES.

           PERFORM 2000-CONSULTA-PARCELAS.

           PERFORM 2016-CARREGA-TELA.

      *----------------------------------------------------------------*
       1215-99-FIM.                    EXIT.
      *----------------------------------------------------------------*
      *----------------------------------------------------------------*
       1216-LIMPAR-LISTA-SAIDA         SECTION.
      *----------------------------------------------------------------*

           PERFORM                     VARYING IND-1
                                       FROM 1
                                       BY 1
                                       UNTIL IND-1
                                       GREATER 12
              MOVE SPACES              TO WRK-O-CINDPC(IND-1)
                                          WRK-O-DTVENC(IND-1)
                                          WRK-O-SLDPRI(IND-1)
                                          WRK-O-SLDENC(IND-1)
                                          WRK-O-SLDTOT(IND-1)
           END-PERFORM.

      *----------------------------------------------------------------*
       1216-99-FIM.                    EXIT.
      *----------------------------------------------------------------*
      *----------------------------------------------------------------*
       1217-RECEBE-VALORES             SECTION.
      *----------------------------------------------------------------*

           MOVE WRK-INP-7951-CHVOPE    TO WRK-I-CHVOPE
                                          WRK-O-CHVOPE

           MOVE WRK-INP-7951-CREFBACEN TO WRK-I-REFBAC
                                          WRK-O-REFBAC
                                          CREFT-RECOR   OF RUECB00D

           MOVE WRK-INP-7951-CJUNC     TO CJUNC-DEPDC   OF RUECB00D

           MOVE WRK-INP-7951-CEDULA    TO CCDULA        OF RUECB00D
           .

      *----------------------------------------------------------------*
       1217-99-FIM.                    EXIT.
      *----------------------------------------------------------------*
      *----------------------------------------------------------------*
       1221-UPDATE-RUECV007            SECTION.
      *----------------------------------------------------------------*

           MOVE AGENCIA                OF NAVEGACAO-RURAL
                                       TO CJUNC-DEPDC OF RUECV007.
           MOVE CCDULA                 OF NAVEGACAO-RURAL
                                       TO CCDULA      OF RUECV007.

           EXEC SQL
             UPDATE DB2PRD.V01CEDULA_TEMPR
             SET CPROG = :RUECV007.CPROG
             WHERE CJUNC_DEPDC = :RUECV007.CJUNC-DEPDC
             AND CCDULA = :RUECV007.CCDULA
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS)
           OR (SQLWARN0                EQUAL 'W')
              MOVE 'DB2'               TO ERR-TIPO-ACESSO
              MOVE 'RUECV007'          TO ERR-DBD-TAB
              MOVE SQLWARN             TO ERR-DBD-TAB(9:8)
              MOVE 'UPDATE'            TO ERR-FUN-COMANDO
              MOVE SQLCODE             TO ERR-SQL-CODE
              MOVE '1221'              TO ERR-LOCAL
              MOVE SPACES              TO ERR-SEGM
              PERFORM 9999-ROTINA-ERRO
           END-IF.

      *----------------------------------------------------------------*
       1221-99-FIM.                    EXIT.
      *----------------------------------------------------------------*
      *----------------------------------------------------------------*
       1222-UPDATE-RUECB042            SECTION.
      *----------------------------------------------------------------*

           MOVE WRK-I-CTERM            TO CTERM            OF RUECB042.

           EXEC SQL
             UPDATE DB2PRD.NAVEGACAO_RURAL
             SET WNAVG_CREDT_RURAL     = :RUECB042.WNAVG-CREDT-RURAL
             WHERE CTERM               = :RUECB042.CTERM
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS AND +100 )
           OR (SQLWARN0                EQUAL 'W'                )
               MOVE 'DB2'              TO ERR-TIPO-ACESSO
               MOVE 'RUECB042'         TO ERR-DBD-TAB
               MOVE SQLWARN            TO ERR-DBD-TAB (9:8)
               MOVE 'UPDATE'           TO ERR-FUN-COMANDO
               MOVE SQLCODE            TO ERR-SQL-CODE
               MOVE '1222'             TO ERR-LOCAL
               MOVE SPACES             TO ERR-SEGM
               PERFORM 9999-ROTINA-ERRO
           END-IF.

      *----------------------------------------------------------------*
       1222-99-FIM.                    EXIT.
      *----------------------------------------------------------------*
      *----------------------------------------------------------------*
       2000-CONSULTA-PARCELAS          SECTION.
      *----------------------------------------------------------------*

      **** REALIZAR CONSULTA NA BASE - PARCELAS RENEGOCIACAO
           PERFORM 2010-SELECT-TABELA-RENEGOC
           .

      *----------------------------------------------------------------*
       2000-99-FIM.                    EXIT.
      *----------------------------------------------------------------*
      *----------------------------------------------------------------*
       2010-SELECT-TABELA-RENEGOC      SECTION.
      *----------------------------------------------------------------*

      **** REALIZAR CONSULTA NA BASE E CARREGA TABELA INTERNA

           PERFORM 2011-OPEN-TABELA-RENEGOC

           PERFORM 2012-FETCH-TABELA-RENEGOC
             UNTIL WRK-FIM-SEL EQUAL 'S'
                OR IND-1       GREATER 120

           PERFORM 2013-CLOSE-TABELA-RENEGOC
           .

      *----------------------------------------------------------------*
       2010-99-FIM.                    EXIT.
      *----------------------------------------------------------------*
      *----------------------------------------------------------------*
       2011-OPEN-TABELA-RENEGOC        SECTION.
      *----------------------------------------------------------------*

           MOVE 0                      TO IND-1
           MOVE 'N'                    TO WRK-FIM-SEL

             EXEC SQL
                OPEN      CSR01-RUECB00D
             END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS )
           OR (SQLWARN0                EQUAL 'W'       )
               MOVE 'DB2'              TO ERR-TIPO-ACESSO
               MOVE 'RUECB00D'         TO ERR-DBD-TAB
               MOVE SQLWARN            TO ERR-DBD-TAB (9:8)
               MOVE 'OPEN'             TO ERR-FUN-COMANDO
               MOVE SQLCODE            TO ERR-SQL-CODE
               MOVE '2011'             TO ERR-LOCAL
               MOVE SPACES             TO ERR-SEGM
               PERFORM 9999-ROTINA-ERRO
           END-IF.
           .

      *----------------------------------------------------------------*
       2011-99-FIM.                    EXIT.
      *----------------------------------------------------------------*
      *----------------------------------------------------------------*
       2012-FETCH-TABELA-RENEGOC       SECTION.
      *----------------------------------------------------------------*

           EXEC SQL
              FETCH CSR01-RUECB00D
              INTO
               :RUECB00D.CREFT-RECOR
              ,:RUECB00D.CTPO-EVNTO-RURAL
              ,:RUECB00D.DEVNTO
              ,:RUECB00D.NPCELA-CONTR
              ,:RUECB00D.DVCTO
              ,:RUECB00D.VSDO-DVDOR-PRINC
              ,:RUECB00D.VTOT-ENCAR
              ,:RUECB00D.VTOT-JURO-MORA
              ,:RUECB00D.VTOT-MULTA
              ,:RUECB00D.CINDCD-PCEL-RENEG
           END-EXEC

           IF SQLCODE                  EQUAL +100
              MOVE 'S'                 TO WRK-FIM-SEL
              MOVE IND-1               TO WRK-TOTAL-REG

           END-IF.

           IF (SQLCODE                 NOT EQUAL ZEROS AND +100)
           OR (SQLWARN0                EQUAL 'W'       )
               MOVE 'DB2'              TO ERR-TIPO-ACESSO
               MOVE 'RUECB00D'         TO ERR-DBD-TAB
               MOVE SQLWARN            TO ERR-DBD-TAB (9:8)
               MOVE 'FETCH'            TO ERR-FUN-COMANDO
               MOVE SQLCODE            TO ERR-SQL-CODE
               MOVE '2012'             TO ERR-LOCAL
               MOVE SPACES             TO ERR-SEGM
               PERFORM 9999-ROTINA-ERRO
           END-IF.

           IF SQLCODE                  EQUAL ZEROS
AQUI          ADD     1                TO IND-1
              PERFORM 2014-MOVE-PARCELAS
           END-IF.
           .

      *----------------------------------------------------------------*
       2012-99-FIM.                    EXIT.
      *----------------------------------------------------------------*
      *----------------------------------------------------------------*
       2013-CLOSE-TABELA-RENEGOC       SECTION.
      *----------------------------------------------------------------*

             EXEC SQL
                CLOSE     CSR01-RUECB00D
             END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS )
           OR (SQLWARN0                EQUAL 'W'       )
               MOVE 'DB2'              TO ERR-TIPO-ACESSO
               MOVE 'RUECB00D'         TO ERR-DBD-TAB
               MOVE SQLWARN            TO ERR-DBD-TAB (9:8)
               MOVE 'CLOSE'            TO ERR-FUN-COMANDO
               MOVE SQLCODE            TO ERR-SQL-CODE
               MOVE '2013'             TO ERR-LOCAL
               MOVE SPACES             TO ERR-SEGM
               PERFORM 9999-ROTINA-ERRO
           END-IF.
           .

      *----------------------------------------------------------------*
       2013-99-FIM.                    EXIT.
      *----------------------------------------------------------------*
      *----------------------------------------------------------------*
       2014-MOVE-PARCELAS              SECTION.
      *----------------------------------------------------------------*

      **** REALIZA CALCULA DE SALDOS DEVEDOR
           PERFORM 2015-CALCULA-SALDOS

      **** DATA VENCIMENTO DA PARCELA
           MOVE DVCTO        OF RUECB00D TO WRK-TAB-DTVENC(IND-1)

      **** INDICADOR DE RENEGOCIACAO
           IF   CINDCD-PCEL-RENEG        OF RUECB00D
                                      EQUAL 'S'
              MOVE 'X'                   TO WRK-TAB-CINDPC(IND-1)
           ELSE
              MOVE SPACES                TO WRK-TAB-CINDPC(IND-1)
           END-IF

      **** NUMERO DA PARCELA DO CONTRATO
           MOVE NPCELA-CONTR OF RUECB00D TO WRK-TAB-NPCELA(IND-1)

AQUI *******DD 1                         *O IND-1
           .

      *----------------------------------------------------------------*
       2014-99-FIM.                    EXIT.
      *----------------------------------------------------------------*
      *----------------------------------------------------------------*
       2015-CALCULA-SALDOS             SECTION.
      *----------------------------------------------------------------*

      **** VALOR PRINCIPAL DA PARCELA
           COMPUTE WRK-TAB-SLDPRI(IND-1) = VSDO-DVDOR-PRINC OF RUECB00D

      **** VALOR DO ENCARGO + JUROS DE MORA + JUROS CA + MULTA
           COMPUTE WRK-TAB-SLDENC(IND-1) = VTOT-ENCAR       OF RUECB00D
                                         + VTOT-JURO-MORA   OF RUECB00D
                                         + VTOT-MULTA       OF RUECB00D

      **** TOTAL DO PRINCIPAL + SOMATóRIA DOS ENCARGOS
           COMPUTE WRK-TAB-SLDTOT(IND-1) = WRK-TAB-SLDPRI(IND-1)
                                         + WRK-TAB-SLDENC(IND-1)
           .

      *----------------------------------------------------------------*
       2015-99-FIM.                    EXIT.
      *----------------------------------------------------------------*
      *----------------------------------------------------------------*
       2016-CARREGA-TELA               SECTION.
      *----------------------------------------------------------------*

      **** VERIFICA INICIO DA TABELA COM RELACAO A PAGINA ATUAL
           COMPUTE IND-4  EQUAL (WRK-PAG-ATUAL - 1) * 12 + 1
AQUI       DISPLAY '&&RUEC7952         IND-4 : ' IND-4
                                                 UPON CONSOLE

      **** VERIFICA ULTIMO REGISTRO REFERENTE A PAGINA ATUAL
           COMPUTE IND-5  EQUAL IND-4 + 11
AQUI       DISPLAY '&&RUEC7952         IND-5 : ' IND-5
                                                 UPON CONSOLE

           MOVE 1            TO IND-1

AQUI       PERFORM                     VARYING IND-4
                                       FROM IND-4
                                       BY 1
                                       UNTIL IND-1
                                       GREATER 12
                                       OR (WRK-TAB-NPCELA(IND-4)
                                       EQUAL ZEROS)

AQUI********ERFORM                      *NTIL IND-4
************                          *REATER IND-5
**************                             OR (WRK-TAB-NPCELA(IND-1)
**************                          *QUAL ZEROS)

              MOVE WRK-TAB-CINDPC(IND-4)   TO WRK-I-CINDPC(IND-1)
                                              WRK-O-CINDPC(IND-1)
              MOVE WRK-TAB-DTVENC(IND-4)   TO WRK-I-DTVENC(IND-1)
                                              WRK-O-DTVENC(IND-1)

AQUI          DISPLAY '&&RUEC7952  TAB-SLDPRI : ' WRK-TAB-SLDPRI(IND-4)
                                                 UPON CONSOLE
AQUI          DISPLAY '&&RUEC7952  TAB-SLDENC : ' WRK-TAB-SLDENC(IND-4)
                                                 UPON CONSOLE
AQUI          DISPLAY '&&RUEC7952  TAB-SLDTOT : ' WRK-TAB-SLDTOT(IND-4)
                                                 UPON CONSOLE

              MOVE WRK-TAB-SLDPRI(IND-4)   TO WRK-S9-15V2
              MOVE WRK-9-08V2              TO WRK-I-SLDPRI-N(IND-1)
                                              WRK-O-SLDPRI-N(IND-1)

              MOVE WRK-TAB-SLDENC(IND-4)   TO WRK-S9-15V2
              MOVE WRK-9-08V2              TO WRK-I-SLDENC-N(IND-1)
                                              WRK-O-SLDENC-N(IND-1)

              MOVE WRK-TAB-SLDTOT(IND-4)   TO WRK-S9-15V2
              MOVE WRK-9-09V2              TO WRK-I-SLDTOT-N(IND-1)
                                              WRK-O-SLDTOT-N(IND-1)

AQUI          DISPLAY '&&RUEC7952  O-SLDPRI-N : ' WRK-O-SLDPRI-N(IND-1)
                                                 UPON CONSOLE
AQUI          DISPLAY '&&RUEC7952  O-SLDENC-N : ' WRK-O-SLDENC-N(IND-1)
                                                 UPON CONSOLE
AQUI          DISPLAY '&&RUEC7952  O-SLDTOT-N : ' WRK-O-SLDTOT-N(IND-1)
                                                 UPON CONSOLE

              ADD 1                        TO IND-1
           END-PERFORM
           .

      *----------------------------------------------------------------*
       2016-99-FIM.                    EXIT.
      *----------------------------------------------------------------*
      *----------------------------------------------------------------*
       3000-PROCESSAR-RUECT798         SECTION.
      *----------------------------------------------------------------*

AQUI       DISPLAY '&&RUEC7952 WRK-MENSAGEM  : ' WRK-MENSAGEM
                                                 UPON CONSOLE

           MOVE WRK-MENSAGEM           TO WRK-INPUT
                                          WRK-COMU-AREA
           MOVE WRK-COMU-NAVEGACAO     TO WRK-INPUT-7951

AQUI       DISPLAY '&&RUEC7952 INPUT-7951    : ' WRK-INPUT-7951
                                                 UPON CONSOLE

AKIIII****-PERFORM 1100-ACESSAR-RUECB042
           PERFORM 3200-DEVOLVER-TELA

AQUI       DISPLAY '&&RUEC7952     WRK-I-PFK : ' WRK-I-PFK
                                                 UPON CONSOLE
           EVALUATE TRUE

      * **     INFORMA PARA PRESSIONAR PF6 PARA SALVAR
               WHEN WRK-I-PFK          EQUAL  '.'
                    PERFORM 3100-TRATAR-ENTER

      * **     PF3 - RETORNA PROGRAMA ANTERIOR
               WHEN WRK-I-PFK          EQUAL  '3'
                    PERFORM 3300-TRATAR-PF3

      * **     PF6 - SALVA MARCACAO/DESMARCACAO DE PARCELAS
               WHEN WRK-I-PFK          EQUAL  '6'
                    PERFORM 3400-TRATAR-PF6

      * **     PF7 - VOLTA PAGINA
               WHEN WRK-I-PFK          EQUAL  '7'
                    PERFORM 3500-TRATAR-PF7

      * **     PF8 - AVANCA PAGINA
               WHEN WRK-I-PFK          EQUAL  '8'
                    PERFORM 3600-TRATAR-PF8

      * **     PF10 - MARCA/DESMARCA TODAS AS PARCELAS NA TELA
               WHEN WRK-I-PFK          EQUAL  'A'
                    PERFORM 3700-TRATAR-PF10

      * **     PF INVALIDA
               WHEN OTHER
                    MOVE WRK-MENS04    TO WRK-O-MENSA

           END-EVALUATE.

      *----------------------------------------------------------------*
       3000-99-FIM.                    EXIT.
      *----------------------------------------------------------------*
      *----------------------------------------------------------------*
       3100-TRATAR-ENTER               SECTION.
      *----------------------------------------------------------------*

           PERFORM 4000-CONSISTIR-AREA-ENTRADA

           IF WRK-INCONSIS             EQUAL 'S'
              MOVE WRK-MENS02          TO WRK-O-MENSA
           ELSE
              MOVE WRK-MENS03          TO WRK-O-MENSA
           END-IF
           .

      *----------------------------------------------------------------*
       3100-99-FIM.                    EXIT.
      *----------------------------------------------------------------*
      *----------------------------------------------------------------*
       3200-DEVOLVER-TELA              SECTION.
      *----------------------------------------------------------------*

           MOVE WRK-I-SENHAS           TO WRK-O-SENHAS.
           MOVE WRK-I-PAGINA           TO WRK-O-PAGINA.
           MOVE WRK-I-CTERM            TO WRK-O-CTERM.
           MOVE WRK-I-CHVOPE           TO WRK-O-CHVOPE.
           MOVE WRK-I-REFBAC           TO WRK-O-REFBAC.

           PERFORM                     VARYING IND-1
                                       FROM 1
                                       BY 1
                                       UNTIL IND-1
                                       GREATER 12
                                       OR (WRK-I-DTVENC(IND-1)
                                       EQUAL SPACES OR LOW-VALUES)
              MOVE WRK-I-CINDPC(IND-1) TO WRK-O-CINDPC(IND-1)
              MOVE WRK-I-DTVENC(IND-1) TO WRK-O-DTVENC(IND-1)
              MOVE WRK-I-SLDPRI(IND-1) TO WRK-O-SLDPRI(IND-1)
              MOVE WRK-I-SLDENC(IND-1) TO WRK-O-SLDENC(IND-1)
              MOVE WRK-I-SLDTOT(IND-1) TO WRK-O-SLDTOT(IND-1)
           END-PERFORM.

           MOVE WRK-I-MENSA            TO WRK-O-MENSA.

      *----------------------------------------------------------------*
       3200-99-FIM.                    EXIT.
      *----------------------------------------------------------------*
      *----------------------------------------------------------------*
       3300-TRATAR-PF3                 SECTION.
      *----------------------------------------------------------------*


           MOVE WRK-CHNG               TO WRK-FUNCAO.
           MOVE LENGTH                 OF WRK-COMU-7951-AREA
                                       TO WRK-COMU-7951-LL.
           MOVE 'RUEC7952'             TO WRK-COMU-7951-TRAN
           MOVE '.'                    TO WRK-COMU-7951-PFK
           MOVE WRK-I-SENHAS           TO WRK-COMU-7951-SENHAS
           MOVE WRK-I-CTERM            TO WRK-COMU-7951-TERM
           MOVE SPACES                 TO WRK-COMU-7951-MENSAGEM
                                          WRK-COMU-7951-PROX-TELA
           MOVE WRK-COMU-7951-AREA     TO WRK-MENSAGEM.

           MOVE 'RUEC7951'             TO WRK-TELA.
AKIII**                                   CPROG OF RUECV007.

AKIII*****-PERFORM 1221-UPDATE-RUECV007.

      *----------------------------------------------------------------*
       3300-99-FIM.                    EXIT.
      *----------------------------------------------------------------*
      *----------------------------------------------------------------*
       3400-TRATAR-PF6                 SECTION.
      *----------------------------------------------------------------*

           PERFORM 4000-CONSISTIR-AREA-ENTRADA

           IF WRK-INCONSIS             EQUAL 'S'
              MOVE WRK-MENS02          TO WRK-O-MENSA
              GO                       TO 3400-99-FIM
           END-IF.

      ***  ATUALIZA INDICADOR MARCADOS OU DESMARCADOS NA TABELA
           PERFORM 3800-ATUALIZA-IND

      ***  ATUALIZA VALOR TOTAL DE RENEGOCIACAO NA TABELA
           PERFORM 3900-ATUALIZA-VTOTRENEG

           MOVE WRK-MENS05             TO WRK-O-MENSA.
           .
      *----------------------------------------------------------------*
       3400-99-FIM.                    EXIT.
      *----------------------------------------------------------------*
      *----------------------------------------------------------------*
       3500-TRATAR-PF7                 SECTION.
      *----------------------------------------------------------------*

           PERFORM 4000-CONSISTIR-AREA-ENTRADA

           IF  WRK-PAG-ATUAL    NOT GREATER 1
               MOVE  WRK-MENS06 TO  WRK-O-MENSA
               PERFORM 3200-DEVOLVER-TELA
           ELSE

             COMPUTE WRK-PAG-ATUAL = WRK-PAG-ATUAL - 1

             PERFORM 1215-MONTAR-TELA

           END-IF

           .
      *----------------------------------------------------------------*
       3500-99-FIM.                    EXIT.
      *----------------------------------------------------------------*
      *----------------------------------------------------------------*
       3600-TRATAR-PF8                 SECTION.
      *----------------------------------------------------------------*

           PERFORM 4000-CONSISTIR-AREA-ENTRADA

           IF  WRK-PAG-ATUAL    EQUAL  WRK-IND-P-MAX
               MOVE  WRK-MENS07 TO  WRK-O-MENSA
               PERFORM 3200-DEVOLVER-TELA
           ELSE

              COMPUTE WRK-PAG-ATUAL = WRK-PAG-ATUAL + 1

              PERFORM 1215-MONTAR-TELA

           END-IF

           .
      *----------------------------------------------------------------*
       3600-99-FIM.                    EXIT.
      *----------------------------------------------------------------*
      *----------------------------------------------------------------*
       3700-TRATAR-PF10                SECTION.
      *----------------------------------------------------------------*

           PERFORM 4000-CONSISTIR-AREA-ENTRADA.

           IF  WRK-INCONSIS            EQUAL 'S'
               MOVE  WRK-MENS09 TO  WRK-O-MENSA
           ELSE
               PERFORM 3710-MARCA-DESMARCA
           END-IF.

           PERFORM 3200-DEVOLVER-TELA.
           .
      *----------------------------------------------------------------*
       3700-99-FIM.                    EXIT.
      *----------------------------------------------------------------*
      *----------------------------------------------------------------*
       3710-MARCA-DESMARCA             SECTION.
      *----------------------------------------------------------------*

           IF WRK-O-CINDPC(IND-1)      EQUAL 'X'
           OR WRK-I-CINDPC(IND-1)      EQUAL 'X'
              MOVE SPACES              TO WRK-INDICE
           ELSE
              MOVE 'X'                 TO WRK-INDICE
           END-IF

      **** VERIFICA INICIO DA TABELA COM RELACAO A PAGINA ATUAL
           COMPUTE IND-4  EQUAL (WRK-PAG-ATUAL - 1) * 12 + 1

      **** VERIFICA ULTIMO REGISTRO REFERENTE A PAGINA ATUAL
           COMPUTE IND-5  EQUAL IND-4 + 11

           PERFORM                     VARYING IND-1
                                       FROM 1
                                       BY 1
                                       UNTIL IND-1
                                       GREATER 12
                                       OR (WRK-O-DTVENC(IND-1)
                                       EQUAL SPACES OR LOW-VALUES)
                                       OR (WRK-I-DTVENC(IND-1)
                                       EQUAL SPACES OR LOW-VALUES)

              MOVE WRK-INDICE          TO WRK-TAB-CINDPC(IND-4)
                                          WRK-O-CINDPC(IND-1)

               ADD 1                   TO IND-4

           END-PERFORM.

           .
      *----------------------------------------------------------------*
       3710-99-FIM.                    EXIT.
      *----------------------------------------------------------------*
      *----------------------------------------------------------------*
       3800-ATUALIZA-IND               SECTION.
      *----------------------------------------------------------------*

           PERFORM 3810-OBTEM-PAG

           PERFORM 3820-UPDATE-TABELA
             UNTIL IND-2 GREATER IND-3

           .
      *----------------------------------------------------------------*
       3800-99-FIM.                    EXIT.
      *----------------------------------------------------------------*
      *----------------------------------------------------------------*
       3810-OBTEM-PAG                  SECTION.
      *----------------------------------------------------------------*

           MOVE 1                        TO IND-2
                                            IND-3

      **** CALCULA QUANTIDADE DE PAGINAS NA TABELA INTERNA
           DIVIDE WRK-TOTAL-REG BY 12 GIVING    WRK-IND-P-MAX
                                      REMAINDER WRK-RESTO
           IF WRK-RESTO > 0
              ADD 1 TO WRK-IND-P-MAX
           END-IF

      **** CALCULA QUAL PRIMEIRO REGISTRO DA TABELA INTERNA P/ ATUALIZAR
           COMPUTE WRK-IND-PRIMEIRO-REG
                                      EQUAL (WRK-PAG-ATUAL - 1)
                                        * 12 + 1

           MOVE    WRK-IND-PRIMEIRO-REG  TO IND-2

      **** CALCULA QUANTIDADE DE REGISTROS NA PAGINA ATUAL
           COMPUTE WRK-FIM-TEORICO    EQUAL WRK-PAG-ATUAL * 12
           IF WRK-TOTAL-REG            LESS WRK-FIM-TEORICO
              COMPUTE IND-3           EQUAL WRK-TOTAL-REG
                                          - WRK-IND-PRIMEIRO-REG + 1
           ELSE
              MOVE 12                    TO IND-3

           END-IF

           .
      *----------------------------------------------------------------*
       3810-99-FIM.                    EXIT.
      *----------------------------------------------------------------*
      *----------------------------------------------------------------*
       3820-UPDATE-TABELA              SECTION.
      *----------------------------------------------------------------*

           IF WRK-TAB-CINDPC(IND-2) EQUAL 'X'
              MOVE 'S'               TO  CINDCD-PCEL-RENEG OF RUECB00D
           ELSE
              MOVE 'N'               TO  CINDCD-PCEL-RENEG OF RUECB00D
           END-IF

           MOVE WRK-TAB-NPCELA(IND-2) TO  NPCELA-CONTR      OF RUECB00D

           EXEC SQL
             UPDATE DB2PRD.TPCELA_TEMPR_RENEG
                SET
                    CINDCD_PCEL_RENEG  = :RUECB00D.CINDCD-PCEL-RENEG
              WHERE CJUNC_DEPDC        = :RUECB00D.CJUNC-DEPDC
                AND CCDULA             = :RUECB00D.CCDULA
                AND CREFT_RECOR        = :RUECB00D.CREFT-RECOR
                AND CTPO_EVNTO_RURAL   = :RUECB00D.CTPO-EVNTO-RURAL
                AND DEVNTO             = :RUECB00D.DEVNTO
                AND NPCELA_CONTR       = :RUECB00D.NPCELA-CONTR
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS )
           OR (SQLWARN0                EQUAL 'W'       )
               MOVE 'DB2'              TO ERR-TIPO-ACESSO
               MOVE 'RUECB00D'         TO ERR-DBD-TAB
               MOVE SQLWARN            TO ERR-DBD-TAB (9:8)
               MOVE 'UPDATE'           TO ERR-FUN-COMANDO
               MOVE SQLCODE            TO ERR-SQL-CODE
               MOVE '3820'             TO ERR-LOCAL
               MOVE SPACES             TO ERR-SEGM
               PERFORM 9999-ROTINA-ERRO
           END-IF.

           IF SQLCODE               EQUAL ZEROS
              ADD 1                    TO IND-2
           END-IF

           .
      *----------------------------------------------------------------*
       3820-99-FIM.                    EXIT.
      *----------------------------------------------------------------*
      *----------------------------------------------------------------*
       3900-ATUALIZA-VTOTRENEG         SECTION.
      *----------------------------------------------------------------*

           PERFORM 3910-OBTEM-VTOT

           PERFORM 3920-UPDATE-VTOTRENEG

           .
      *----------------------------------------------------------------*
       3900-99-FIM.                    EXIT.
      *----------------------------------------------------------------*
      *----------------------------------------------------------------*
       3910-OBTEM-VTOT                 SECTION.
      *----------------------------------------------------------------*

           PERFORM                      VARYING IND-1
                                        FROM 1
                                        BY 1
                                        UNTIL IND-1
                                        GREATER 120
                                        OR (WRK-TAB-NPCELA(IND-1)
                                        EQUAL ZEROS)

              IF WRK-TAB-CINDPC(IND-1)  EQUAL 'X'
                 COMPUTE WRK-VRENEG-CONTR-RURAL
                                           = WRK-TAB-SLDPRI(IND-1)
                                           + WRK-TAB-SLDENC(IND-1)
                                           + WRK-TAB-SLDTOT(IND-1)
              END-IF
              ADD 1                        TO IND-1
           END-PERFORM

           .
      *----------------------------------------------------------------*
       3910-99-FIM.                    EXIT.
      *----------------------------------------------------------------*
      *----------------------------------------------------------------*
       3920-UPDATE-VTOTRENEG           SECTION.
      *----------------------------------------------------------------*

           MOVE AGENCIA               OF NAVEGACAO-RURAL
                                      TO CJUNC-DEPDC     OF RUECB00C
           MOVE CCDULA                OF NAVEGACAO-RURAL
                                      TO CCDULA          OF RUECB00C
           MOVE CREFT-RECOR           OF RUECB00D
                                      TO CREFT-RECOR     OF RUECB00C

           EXEC SQL
             UPDATE DB2PRD.TCDULA_TEMPR_RENEG
                SET
                    VRENEG_CONTR_RURAL = :WRK-VRENEG-CONTR-RURAL
              WHERE
                    CJUNC_DEPDC        = :RUECB00C.CJUNC-DEPDC
                AND CCDULA             = :RUECB00C.CCDULA
                AND CREFT_RECOR        = :RUECB00C.CREFT-RECOR
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS )
           OR (SQLWARN0                EQUAL 'W'       )
               MOVE 'DB2'              TO ERR-TIPO-ACESSO
               MOVE 'RUECB00D'         TO ERR-DBD-TAB
               MOVE SQLWARN            TO ERR-DBD-TAB (9:8)
               MOVE 'UPDATE'           TO ERR-FUN-COMANDO
               MOVE SQLCODE            TO ERR-SQL-CODE
               MOVE '3920'             TO ERR-LOCAL
               MOVE SPACES             TO ERR-SEGM
               PERFORM 9999-ROTINA-ERRO
           END-IF.

           IF SQLCODE               EQUAL ZEROS
              ADD 1                    TO IND-2
           END-IF

           .
      *----------------------------------------------------------------*
       3920-99-FIM.                    EXIT.
      *----------------------------------------------------------------*
      *----------------------------------------------------------------*
       4000-CONSISTIR-AREA-ENTRADA     SECTION.
      *----------------------------------------------------------------*

           PERFORM 4100-CONSISTIR-CAMPOS.

      *----------------------------------------------------------------*
       4000-99-FIM.                    EXIT.
      *----------------------------------------------------------------*
      *----------------------------------------------------------------*
       4100-CONSISTIR-CAMPOS           SECTION.
      *----------------------------------------------------------------*

           MOVE 'N'                    TO WRK-INCONSIS.

           PERFORM                     VARYING IND-1
                                       FROM 1
                                       BY 1
                                       UNTIL IND-1
                                       GREATER 12
                                       OR (WRK-O-DTVENC(IND-1)
                                       EQUAL SPACES OR LOW-VALUES)
                                       OR (WRK-I-DTVENC(IND-1)
                                       EQUAL SPACES OR LOW-VALUES)
              IF (WRK-O-CINDPC(IND-1)  NOT EQUAL SPACES AND 'X'
                                       AND LOW-VALUES)           AND
                 (WRK-I-CINDPC(IND-1)  NOT EQUAL SPACES AND 'X'
                                       AND LOW-VALUES)

                 MOVE 'S'              TO WRK-INCONSIS
                 MOVE TAB-ATRIBUTO(3)  TO WRK-O-CINDPC-ATTR(IND-1)
              END-IF

           END-PERFORM.

      *----------------------------------------------------------------*
       4100-99-FIM.                    EXIT.
      *----------------------------------------------------------------*
      *----------------------------------------------------------------*
       7000-PROCESSAR-BRAD0660         SECTION.
      *----------------------------------------------------------------*

           MOVE LENGTH OF WRK-OUTPUT   TO WRK-660-LL-OUTPUT
                                          WRK-O-LL
           MOVE LENGTH OF WRK-660-RUECT798
                                       TO WRK-660-LL

           CALL  'BRAD0660'            USING WRK-OUTPUT
                                             WRK-660-RUECT798.

           IF RETURN-CODE              NOT EQUAL  ZEROS
              MOVE  'APL'              TO ERR-TIPO-ACESSO
              MOVE  RETURN-CODE        TO WRK-RETURN-CODE
              MOVE  70                 TO WRK-LOCAL-ERRO
              MOVE  WRK-ERRO-BRAD0660  TO ERR-TEXTO
              PERFORM  9999-ROTINA-ERRO
           END-IF.

           MOVE  WRK-ISRT              TO WRK-FUNCAO.
           MOVE  WRK-OUTPUT            TO WRK-MENSAGEM.

      *----------------------------------------------------------------*
       7000-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       8000-FINALIZAR                  SECTION.
      *----------------------------------------------------------------*

AQUI       DISPLAY '&&RUEC7952 - 8000-FINALIZAR : ' RETURN-CODE
                                                 UPON CONSOLE
           GOBACK.

      *----------------------------------------------------------------*
       8000-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       9999-ROTINA-ERRO                SECTION.
      *----------------------------------------------------------------*

           IF ERR-MODULO               NOT EQUAL  SPACES  AND
                                                  LOW-VALUES
              MOVE  'RUEC7952'         TO  ERR-PGM
           ELSE
              MOVE  'SENHAS02'         TO  ERR-PGM
              MOVE  'RUEC7952'         TO  ERR-MODULO
           END-IF.

           MOVE  WRK-COD-USER-X        TO  ERR-COD-USER.
           MOVE  WRK-COD-DEPTO         TO  ERR-COD-DEPTO.


           IF  ERR-TIPO-ACESSO         EQUAL 'DB2'
               CALL  'BRAD7100'        USING  LNK-IO-PCB
                                              ERRO-AREA
                                              LNK-ALT-PCB
                                              SQLCA
           ELSE
               CALL 'BRAD7100'         USING  LNK-IO-PCB
                                              ERRO-AREA
                                              LNK-ALT-PCB
           END-IF.

           GOBACK.

      *----------------------------------------------------------------*
       9999-99-FIM.                    EXIT.
      *----------------------------------------------------------------*
