      *================================================================*
       IDENTIFICATION                  DIVISION.
      *================================================================*
       PROGRAM-ID. BVVE1809.
       AUTHOR.     DOUGLAS LIRA.
      *================================================================*
      *                        P R O C W O R K                         *
      *----------------------------------------------------------------*
      *                                                                *
      *    PROGRAMA     : BVVE1809                                     *
      *    PROGRAMADOR  : DOUGLAS LIRA         - PROCWORK              *
      *    ANALISTA     : SERGIO VIEIRA JUNIOR - PROCWORK              *
      *    DATA         : 27/03/2010                                   *
      *                                                                *
      *    OBJETIVO     :                                              *
      *      CARTAO DE CREDITO - VISA VALE - INCLUSAO DE PEDIDO DE     *
      *      CARTAO                                                    *
      *                                                                *
      *    MAPA/MAPSET  :                                              *
      *      BVVE809 - INCLUSAO DE PEDIDO DE CARTAO                    *
      *                                                                *
      *    TRANSACAO    :                                              *
      *      VV09                                                      *
      *                                                                *
      *    ARQUIVOS     :                                              *
      *      DDNAME                               INCLUDE/BOOK         *
      *      VVPEDICT                               I#BVVEPC           *
4SYSFB*      SPLIMTEC                               I#BVVESL           *
      *                                                                *
      *    NAVEGACAO    :                                              *
      *      CHAMADO POR: BVVE1807              CHAMA: BVVE1807        *
      *                                                BVVE1804        *
      *                                                BVVE2950        *
      *================================================================*

4SYSFB*================================================================*
.     *                       A L T E R A C A O                        *
.     *================================================================*
.     *    PROGRAMADOR.: FABRICA         - FOURSYS                     *
.     *    DATA........: 08/08/2018                                    *
.     *----------------------------------------------------------------*
.     *    PROJETO.....: PROPOSTA ELETRONICA FASE II PAGAMENTO A PRAZO *
.     *    OBJETIVO....: GRAVAR ARQUIVO DE LIMITES PARA PPSTAS CONE    *
4SYSFB*----------------------------------------------------------------*
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
       77  FILLER                      PIC  X(32)        VALUE
           '*  INICIO DA WORKING BVVE1809  *'.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       77  FILLER                      PIC  X(32)        VALUE
           '*         INDEXADORES          *'.
      *----------------------------------------------------------------*
       01  WRK-INDEXADORES.
           03  WRK-IND                 PIC  9(05) COMP-3 VALUE ZEROS.
           03  WRK-IND2                PIC  9(05) COMP-3 VALUE ZEROS.
           03  WRK-IND3                PIC  9(05) COMP-3 VALUE ZEROS.
4SYSFB     03  IND-4                   PIC  9(05) COMP-3 VALUE ZEROS.

      *----------------------------------------------------------------*
       77  FILLER                      PIC  X(32)        VALUE
           '*     VARIAVEIS AUXILIARES     *'.
      *----------------------------------------------------------------*
       01  WRK-AUXILIARES.
           03  WRK-ABSTIME             PIC S9(15) COMP-3 VALUE ZEROS.
           03  WRK-LENGTH              PIC S9(04) COMP   VALUE ZEROS.
           03  WRK-PROGRAMA            PIC  X(08)        VALUE SPACES.
           03  WRK-INCONSISTENTE       PIC  X(01)        VALUE SPACES.

           03  WRK-ITEM-TS-VV05        PIC S9(04) COMP   VALUE ZEROS.
           03  WRK-NOME-TS-VV05.
               05  WRK-TERMID-VV05     PIC  X(04)        VALUE SPACES.
               05  WRK-TRANID-VV05     PIC  X(04)        VALUE SPACES.

           03  WRK-DDD-BIF             PIC  X(02)        VALUE SPACES.
           03  FILLER        REDEFINES WRK-DDD-BIF.
               05  WRK-DDD-BIF-R       PIC  9(02).

           03  WRK-CPF-AUX.
               05  WRK-CPF             PIC  9(09)        VALUE ZEROS.
               05  WRK-CONTROLE        PIC  9(02)        VALUE ZEROS.
           03  WRK-CPF-AUX-R           REDEFINES WRK-CPF-AUX
                                       PIC  9(11).

           03  WRK-CNPJ-AUX.
7C2511*        05  WRK-CNPJ            PIC  9(08)        VALUE ZEROS.
7C2511*        05  WRK-FILIAL          PIC  9(04)        VALUE ZEROS.
7C2511         05  WRK-CNPJ            PIC  X(08)        VALUE SPACES.
7C2511         05  WRK-FILIAL          PIC  X(04)        VALUE SPACES.
               05  WRK-CONTROLE-CNPJ   PIC  9(02)        VALUE ZEROS.
7C2511*    03  WRK-CNPJ-AUX-R          REDEFINES WRK-CNPJ-AUX
7C2511*                                PIC  9(14).
7C2511     03  WRK-CNPJ-AUX-R          REDEFINES WRK-CNPJ-AUX
                                       PIC  X(14).

           03  WRK-CNPJ-CALCULAR1.
7C2511*        05  WRK-NCNPJ-CALC1     PIC  9(09)        VALUE ZEROS.
7C2511         05  WRK-NCNPJ-CALC1     PIC  X(09)        VALUE SPACES.
               05  WRK-FILIAL-CALC1    PIC  X(04)        VALUE SPACES.

           03  WRK-CNPJ-CALCULAR2.
7C2511*        05  WRK-NCNPJ-CALC2     PIC  9(09)        VALUE ZEROS.
7C2511         05  WRK-NCNPJ-CALC2     PIC  X(09)        VALUE SPACES.
               05  WRK-FILIAL-CALC2    PIC  X(04)        VALUE SPACES.
               05  WRK-RETORNADO       PIC  X(01)        VALUE SPACES.

           03  WRK-DIGITO-RETORNADO.
               05  WRK-DIG-RET1        PIC  X(01)        VALUE SPACES.
               05  WRK-DIG-RET2        PIC  X(01)        VALUE SPACES.

           03  WRK-DIGITO-AUX.
               05  WRK-DIGITO-N        PIC  9(02)        VALUE ZEROS.

           03  WRK-DATA-AUX.
               05  WRK-DIA-AUX         PIC  9(02)        VALUE ZEROS.
               05  WRK-MES-AUX         PIC  9(02)        VALUE ZEROS.
               05  WRK-ANO-AUX         PIC  9(04)        VALUE ZEROS.
           03  WRK-DATA-AUX-R REDEFINES WRK-DATA-AUX
                                       PIC  9(08).

           03  WRK-DATA-AUX-INV.
               05  WRK-ANO-AUX-INV-X.
                   07  WRK-ANO-AUX-INV PIC  9(04)        VALUE ZEROS.
               05  WRK-MES-AUX-INV-X.
                   07  WRK-MES-AUX-INV PIC  9(02)        VALUE ZEROS.
               05  WRK-DIA-AUX-INV-X.
                   07  WRK-DIA-AUX-INV PIC  9(02)        VALUE ZEROS.
           03  WRK-DATA-AUX-INV-R      REDEFINES WRK-DATA-AUX-INV
                                       PIC  9(08).

           03  WRK-AREA-TS-VV05.
               05  WRK-TAB-TARIFAS-VV05 OCCURS 12.
                   07  WRK-TAB-COD-VV05-X.
                       09  WRK-TAB-COD-VV05
                                       PIC  9(03).
                   07  WRK-TAB-DESC-VV05
                                       PIC  X(40).
                   07  WRK-TAB-VLTAR-VV05
                                       PIC  9(03)V99.
4SYSFB             07  WRK-TAB-TARIF-OB-VV05  PIC X(01).
4SYSFB             07  WRK-TAB-TARIF-FO-VV05  PIC X(01).

           03  WRK-COD-FUNC-11         PIC  9(011)       VALUE ZEROS.
           03  FILLER  REDEFINES WRK-COD-FUNC-11.
               05  FILLER              PIC  9(004).
               05  WRK-COD-FUNC-7      PIC  9(007).

           03  WRK-VL-MED-AUX          PIC  9(009)V99      VALUE ZEROS.
           03  FILLER                  REDEFINES WRK-VL-MED-AUX.
               05  FILLER              PIC  9(001).
               05  WRK-VL-MED-R        PIC  9(008)V99.

4SYSFB     03  WRK-USUARIO             PIC  X(008)       VALUE SPACES.
.          03  WRK-DATA-X              PIC  X(010)       VALUE SPACES.
.          03  FILLER                  REDEFINES WRK-DATA-X.
.              05  WRK-DIA-X           PIC  9(002).
.              05  FILLER              PIC  9(001).
.              05  WRK-MES-X           PIC  9(002).
.              05  FILLER              PIC  9(001).
.              05  WRK-ANO-X           PIC  9(004).
.
.          03  WRK-DATA-FIM            PIC  9(008)       VALUE ZEROS.
.          03  FILLER                  REDEFINES WRK-DATA-FIM.
.              05  WRK-DIA-FIM         PIC  9(002).
.              05  WRK-MES-FIM         PIC  9(002).
.              05  WRK-ANO-FIM         PIC  9(004).

.      01  WRK-EMAIL                   PIC  X(40)        VALUE SPACES.
.      01  FILLER  REDEFINES WRK-EMAIL.
.          05  WRK-DIG-EMAIL           PIC  X(01) OCCURS 40.
.
.      01  WRK-PONTO                   PIC  X(01)        VALUE SPACES.
.
4SYSFB 01  WRK-VALIDAR-EMAIL           PIC  X(01)        VALUE SPACES.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(32)        VALUE
           '*  AREA PARA POOL3205  *'.
      *----------------------------------------------------------------*

       01  WRK-AREA-POOL3205.
           03  WRK-ENVIADA             PIC  9(09) COMP-3 VALUE ZEROS.
           03  WRK-OPCAO               PIC  X(01)        VALUE SPACES.
           03  WRK-DATA-JULIANA        PIC  9(07) COMP-3 VALUE ZEROS.
           03  WRK-DATA-EDITADA        PIC  X(10)        VALUE SPACES.
           03  WRK-DATA-GREGORIANA     PIC  9(08)        VALUE ZEROS.
           03  WRK-DIA-SEMANA          PIC  X(13)        VALUE SPACES.
           03  WRK-MES-EXTENSO         PIC  X(09)        VALUE SPACES.
           03  WRK-DIA-UTIL-ANTERIOR   PIC  9(08)        VALUE ZEROS.
           03  WRK-DIA-UTIL-POSTERIOR  PIC  9(08)        VALUE ZEROS.
           03  WRK-MENSAGEM            PIC  X(50)        VALUE SPACES.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(32)        VALUE
           '*  AREA PARA POOL1911  *'.
      *----------------------------------------------------------------*

       01  WRK-AREA-POOL1911.
           05  WRK-POOL1911-VALOR      PIC  X(30)        VALUE SPACES.
           05  WRK-POOL1911-DIGITO     PIC  X(01)        VALUE SPACES.
           05  WRK-POOL1911-TAMANHO    PIC  9(02)        VALUE ZEROS.
           05  WRK-POOL1911-RETORNO    PIC  9(02)        VALUE ZEROS.
           05  WRK-POOL1911-MENSAGEM.
               10  WRK-P1911-COD-MENSAGEM
                                       PIC  9(03)        VALUE ZEROS.
               10  FILLER              PIC  X(01)        VALUE SPACES.
               10  WRK-P1911-MENSAGEM  PIC  X(79)        VALUE SPACES.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(32)        VALUE
           '*      AREA DE MENSAGENS       *'.
      *----------------------------------------------------------------*

       01  WRK-MENSAGENS.
           03  WRK-MSG01               PIC  X(79)        VALUE
               'PF INVALIDA'.

           03  WRK-MSG02               PIC  X(79)        VALUE
               'TRANSACAO ENCERRADA. ENTRADA SOMENTE VIA MENU'.

           03  WRK-MSG03               PIC  X(79)        VALUE
               'DIGITE OS DADOS E TECLE <ENTER>'.

           03  WRK-MSG04               PIC  X(79)        VALUE
               'INFORME O NOME DE GRAVACAO NOS CARTOES'.

           03  WRK-MSG05               PIC  X(79)        VALUE
               'INFORME "S" OU "N"'.

           03  WRK-MSG06               PIC  X(79)        VALUE
               'FILIAL NAO NUMERICA OU IGUAL A ZEROS'.

           03  WRK-MSG07               PIC  X(79)        VALUE
               'DIGITO DE CONTROLE NAO NUMERICO'.

           03  WRK-MSG08               PIC  X(79)        VALUE
               'DIGITO DE CONTROLE INVALIDO'.

           03  WRK-MSG09               PIC  X(79)        VALUE
               'PREENCHA APENAS SE ENTREGA FOR CENTRALIZADA'.

           03  WRK-MSG10               PIC  X(79)        VALUE
               'PREENCHA APENAS SE FATURAMENTO FOR CENTRALIZADO'.

           03  WRK-MSG11               PIC  X(79)        VALUE
               'CPF NAO NUMERICO OU IGUAL A ZEROS'.

           03  WRK-MSG12               PIC  X(79)        VALUE
               'DATA DE NASCIMENTO INVALIDA'.

           03  WRK-MSG13               PIC  X(79)        VALUE
               'INFORME O NOME DO INTERLOCUTOR'.

           03  WRK-MSG14               PIC  X(79)        VALUE
               'DDD NAO NUMERICO OU IGUAL A ZEROS'.

           03  WRK-MSG15               PIC  X(79)        VALUE
               'TELEFONE NAO NUMERICO OU IGUAL A ZEROS'.

           03  WRK-MSG16               PIC  X(79)        VALUE
               'TECLE PF6 PARA CONFIRMAR INCLUSAO DO PEDIDO DE CARTAO OU
      -        ' <ENTER> PARA CANCELAR'.

           03  WRK-MSG17               PIC  X(79)        VALUE
               'INCLUSAO EFETUADA COM SUCESSO'.

           03  WRK-MSG18               PIC  X(79)        VALUE
               'CPF NAO NUMERICO'.

           03  WRK-MSG19               PIC  X(79)        VALUE
               'CODIGO FUNCIONAL NAO NUMERICO'.

           03  WRK-MSG20               PIC  X(79)        VALUE
               'PREENCHER CPF OU CODIGO FUNCIONAL DO VENDEDOR'.

4SYSFB     03  WRK-MSG21               PIC  X(79)        VALUE
.              'INFORME O EMAIL DO INTERLOCUTOR'.
.
.          03  WRK-MSG22               PIC  X(79)        VALUE
.              'PRIMEIRO CARACTER DO E-MAIL DEVE SER LETRA OU NUMERO'.
.
.          03  WRK-MSG23               PIC  X(79)        VALUE
.              'ULTIMO CARACTER DO E-MAIL DEVE SER LETRA OU NUMERO'.
.
.          03  WRK-MSG24               PIC  X(79)        VALUE
.              'E-MAIL DEVE POSSUIR O CARACTER @(ARROBA)'.
.
.          03  WRK-MSG25               PIC  X(79)        VALUE
.              'E-MAIL COM CARACTER INVALIDO ANTES DO @(ARROBA)'.
.
.          03  WRK-MSG26               PIC  X(79)        VALUE
.              'E-MAIL COM CARACTER INVALIDO APOS @(ARROBA)'.
.
.          03  WRK-MSG27               PIC  X(79)        VALUE
.              'E-MAIL COM CARACTER INVALIDO ANTES DO PONTO'.
.
.          03  WRK-MSG28               PIC  X(79)        VALUE
.              'E-MAIL COM CARACTER INVALIDO APOS O PONTO'.
.
.          03  WRK-MSG29               PIC  X(79)        VALUE
.              'OBRIGATORIO PREENCHER COM E-MAIL VALIDO'.
.
.          03  WRK-MSG30               PIC  X(79)        VALUE
.              'E-MAIL PREENCHIDO COM CARACTER INVALIDO'.
.
.          03  WRK-MSG31               PIC  X(79)        VALUE
4SYSFB         'EMAIL INVALIDO'.

           03  WRK-MSG99.
               05  FILLER              PIC  X(35)        VALUE
                   'ERRO NAO IDENTIF PELO PGM. EIBRESP='.
               05  WRK-EIBRESP         PIC  9(04)        VALUE ZEROS.
               05  FILLER              PIC  X(09)        VALUE
                   ' E LOCAL='.
               05  WRK-LOCAL-EIBRESP   PIC  9(02)        VALUE ZEROS.
               05  FILLER              PIC  X(20)        VALUE
                   '. CONTACTAR ANALISTA'.

           03  WRK-MSG00.
               05  FILLER              PIC  X(19)        VALUE
                   'ERRO CICS - ABCODE='.
               05  WRK-ABCODE-MSG      PIC  X(08)        VALUE SPACES.
               05  FILLER              PIC  X(09)        VALUE
                   ' E LOCAL='.
               05  WRK-LOCAL-ABCODE    PIC  9(02)        VALUE ZEROS.
               05  FILLER              PIC  X(30)        VALUE
                   '. CONTACTAR ANALISTA'.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(32)        VALUE
           '*      COMMAREA AUXILIAR       *'.
      *----------------------------------------------------------------*

           COPY BVVEWCMA.
      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(32)        VALUE
           '*     AREA DO MAPA BVVE809     *'.
      *----------------------------------------------------------------*

           COPY BVVE809.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(32)        VALUE
           '*   AREA DO ARQUIVO VVPEDICT  *'.
      *----------------------------------------------------------------*

           COPY 'I#BVVEPC'.

4SYSFB*----------------------------------------------------------------*
  .    01  FILLER                      PIC  X(32)        VALUE
  .        '*   AREA DO ARQUIVO SPLIMTEC  *'.
  .   *----------------------------------------------------------------*
  .
  .        COPY 'I#BVVESL'.
4SYSFB
      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(32)        VALUE
           '*   FIM DA WORKING BVVE1809   *'.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       LINKAGE                         SECTION.
      *----------------------------------------------------------------*

       01  DFHCOMMAREA.
           03  FILLER                  PIC  X(1110).

      *================================================================*
       PROCEDURE                       DIVISION USING DFHCOMMAREA.
      *================================================================*

      *----------------------------------------------------------------*
       0000-ROTINA-PRINCIPAL           SECTION.
      *----------------------------------------------------------------*

           MOVE    01                  TO WRK-LOCAL-EIBRESP
                                          WRK-LOCAL-ABCODE

           EXEC CICS
                HANDLE ABEND
                       LABEL(9998-TRATAR-ERRO)
           END-EXEC

           IF EIBRESP                  NOT EQUAL DFHRESP(NORMAL)
              PERFORM 9998-TRATAR-ERRO
           END-IF

           MOVE    02                  TO WRK-LOCAL-EIBRESP
                                          WRK-LOCAL-ABCODE

           EXEC CICS
                HANDLE AID
                       PF2   (0100-TRATAR-PF2)
                       PF3   (0200-TRATAR-PF3)
                       PF6   (0600-TRATAR-PF6)
                       PF9   (0700-TRATAR-PF9)
                       ENTER (0800-TRATAR-ENTER)
                       CLEAR (0200-TRATAR-PF3)
                       ANYKEY(0900-TRATAR-PF-INVALIDA)
           END-EXEC

           IF EIBRESP                  NOT EQUAL DFHRESP(NORMAL)
              PERFORM 9998-TRATAR-ERRO
           END-IF

           INITIALIZE WRK-AUXILIARES

           MOVE    EIBTRMID            TO WRK-TERMID-VV05
           MOVE   'VV05'               TO WRK-TRANID-VV05

           IF (EIBCALEN                EQUAL ZEROS) OR
              (EIBTRNID                NOT EQUAL 'VV08' AND 'VV09')
              PERFORM 0300-ENCERRAR-TRANSACAO
           END-IF

           MOVE    DFHCOMMAREA(1:EIBCALEN)
                                       TO WRK-COMMAREA

           IF EIBTRNID                 NOT EQUAL 'VV09'
              PERFORM 0400-RECEBER-CONTROLE

           ELSE
              PERFORM 0500-RECEBER-MAPA
           END-IF.

      *----------------------------------------------------------------*
       0000-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       0100-TRATAR-PF2                 SECTION.
      *----------------------------------------------------------------*

           MOVE    03                  TO WRK-LOCAL-EIBRESP
                                          WRK-LOCAL-ABCODE

           EXEC CICS
                ISSUE DISCONNECT
           END-EXEC.

           IF EIBRESP                  NOT EQUAL DFHRESP(NORMAL)
              PERFORM 9998-TRATAR-ERRO
           END-IF

           EXEC CICS
                RETURN
           END-EXEC.

      *----------------------------------------------------------------*
       0100-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       0200-TRATAR-PF3                 SECTION.
      *----------------------------------------------------------------*

           MOVE    04                  TO WRK-LOCAL-EIBRESP
                                          WRK-LOCAL-ABCODE

           EXEC CICS
                XCTL PROGRAM ('BVVE1808')
                     COMMAREA(WRK-COMMAREA)
           END-EXEC

           IF EIBRESP                  NOT EQUAL DFHRESP(NORMAL)
              PERFORM 9998-TRATAR-ERRO
           END-IF.

      *----------------------------------------------------------------*
       0200-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       0300-ENCERRAR-TRANSACAO         SECTION.
      *----------------------------------------------------------------*

           EXEC CICS SEND
                FROM(WRK-MSG02)
                ERASE
           END-EXEC.

           EXEC CICS
                RETURN
           END-EXEC.

      *----------------------------------------------------------------*
       0300-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       0400-RECEBER-CONTROLE           SECTION.
      *----------------------------------------------------------------*

           MOVE    LOW-VALUES          TO BVVE809I
           MOVE   '1'                  TO WRK-COMM-FASE

           MOVE    WRK-COMM-CNPJ       TO CNPJO
           MOVE    WRK-COMM-FILIAL     TO FILIALO
           MOVE    WRK-COMM-CONTROL    TO CONTROLO

           IF WRK-COMM-NOME-GRAVACAO   NOT EQUAL SPACES AND LOW-VALUES
              MOVE WRK-COMM-NOME-GRAVACAO
                                       TO NOMEGRO
              MOVE WRK-COMM-ENTREGA-CENTR
                                       TO ENTREGAO

              IF WRK-COMM-ENTREGA-CENTR
                                       EQUAL 'S'
                 MOVE WRK-COMM-FILIALE TO FILIALEO
                 MOVE WRK-COMM-CONTROLE
                                       TO CONTREO
              END-IF

              MOVE WRK-COMM-FATURA-CENTR
                                       TO FATURAO

              IF WRK-COMM-FATURA-CENTR EQUAL 'S'
                 MOVE WRK-COMM-FILIALF TO FILIALFO
                 MOVE WRK-COMM-CONTROLF
                                       TO CONTRFO
                 MOVE WRK-COMM-FILIALC TO FILIALCO
                 MOVE WRK-COMM-CONTROLC
                                       TO CONTRCO
              END-IF

              MOVE WRK-COMM-FUNCD      TO FUNCDO
              MOVE WRK-COMM-CPFD       TO CPFDO
              MOVE WRK-COMM-CONTROLD   TO CONTRDO
              MOVE WRK-COMM-DTNASCD    TO WRK-DATA-AUX-R
              MOVE WRK-DIA-AUX         TO DDNASCDO
              MOVE WRK-MES-AUX         TO MMNASCDO
              MOVE WRK-ANO-AUX         TO AANASCDO
              MOVE WRK-COMM-NOMED      TO NOMEDO
              MOVE WRK-COMM-DDDD       TO DDDDO
              MOVE WRK-COMM-TELD       TO TELDO
              MOVE WRK-COMM-MESMOS     TO MESMOSO
              MOVE WRK-COMM-FUNCO      TO FUNCOO
              MOVE WRK-COMM-CPFO       TO CPFOO
              MOVE WRK-COMM-CONTROLO   TO CONTROPO
              MOVE WRK-COMM-DTNASCO    TO WRK-DATA-AUX-R
              MOVE WRK-DIA-AUX         TO DDNASCOO
              MOVE WRK-MES-AUX         TO MMNASCOO
              MOVE WRK-ANO-AUX         TO AANASCOO
              MOVE WRK-COMM-NOMEO      TO NOMEOO
              MOVE WRK-COMM-DDDO       TO DDDOO
              MOVE WRK-COMM-TELO       TO TELOO
              MOVE WRK-COMM-CPF3       TO CPF3O
              MOVE WRK-COMM-CONTROL3   TO CONTR3O
              MOVE WRK-COMM-CODFUNC    TO CODFUNCO
4SYSFB        MOVE WRK-COMM-EMAIL      TO EMAILO
4SYSFB        MOVE WRK-COMM-EMAIL2     TO EMAIL2O
           END-IF

           MOVE   -1                   TO NOMEGRL
           MOVE    WRK-MSG03           TO MENSAGO

           PERFORM 0410-ENVIAR-MAPA.

      *----------------------------------------------------------------*
       0400-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       0410-ENVIAR-MAPA                SECTION.
      *----------------------------------------------------------------*

           EXEC CICS
                ASKTIME ABSTIME (WRK-ABSTIME)
           END-EXEC

           EXEC CICS
                FORMATTIME ABSTIME (WRK-ABSTIME)
                           DATESEP ('/')
                           DDMMYY  (DTASISO)
                           TIMESEP (':')
                           TIME    (HORSISO)
           END-EXEC.

           MOVE    05                  TO WRK-LOCAL-EIBRESP
                                          WRK-LOCAL-ABCODE

           EXEC CICS
                SEND MAP   ('BVVE809')
                     MAPSET('BVVE809')
                     FROM  (BVVE809O)
                     CURSOR
                     ERASE
                     FREEKB
           END-EXEC.

           IF EIBRESP                  NOT EQUAL DFHRESP(NORMAL)
              MOVE    EIBRESP          TO WRK-EIBRESP
              EXEC CICS
                   SEND FROM(WRK-MSG99)
                        ERASE
              END-EXEC

              EXEC CICS
                   RETURN
              END-EXEC

           ELSE
              EXEC CICS
                   RETURN TRANSID ('VV09')
                          COMMAREA(WRK-COMMAREA)
              END-EXEC
           END-IF.

      *----------------------------------------------------------------*
       0410-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       0500-RECEBER-MAPA               SECTION.
      *----------------------------------------------------------------*

           MOVE    06                  TO WRK-LOCAL-EIBRESP
                                          WRK-LOCAL-ABCODE

           EXEC CICS
                RECEIVE MAP   ('BVVE809')
                        MAPSET('BVVE809')
                        INTO  (BVVE809I)
           END-EXEC.

           IF EIBRESP                  NOT EQUAL DFHRESP(NORMAL)
              PERFORM 9998-TRATAR-ERRO
           END-IF.

      *----------------------------------------------------------------*
       0500-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       0600-TRATAR-PF6                 SECTION.
      *----------------------------------------------------------------*

           IF   WRK-COMM-FASE          EQUAL '1'
                PERFORM 0900-TRATAR-PF-INVALIDA
           END-IF.


           INITIALIZE                  CADA-PEDIDO-CARTAO
           MOVE    WRK-COMM-CNPJ       TO CADA-PDCT-CH-CNPJ-CONTR
           MOVE    WRK-COMM-FILIAL     TO CADA-PDCT-CH-FILIAL-CONTR
           MOVE    WRK-COMM-CONTROL    TO CADA-PDCT-CH-CTRL-CONTR
           MOVE    WRK-COMM-PROD       TO CADA-PDCT-CH-PRODUTO
           MOVE    ZEROS               TO CADA-NRO-CONTRATO

4SYSFB     IF      WRK-COMM-OUTROS     EQUAL 'X'
  .                MOVE   'L'          TO CADA-STATUS
  .        ELSE
  .                MOVE   'P'          TO CADA-STATUS
4SYSFB     END-IF

           MOVE    WRK-COMM-NOME-EMP   TO CADA-RAZAO-SOCIAL
           MOVE    WRK-COMM-NOME-FAN   TO CADA-NOME-FANTASIA
           MOVE    WRK-COMM-TIPOEMP    TO CADA-TIPO-EMPRESA
4SYSFB     MOVE    ZEROS               TO CADA-REST-EMPRESA
           MOVE    WRK-COMM-QTD-FUNC   TO CADA-QTD-FUNC-EMPRESA
           MOVE    WRK-COMM-NOME-FIL   TO CADA-NOME-FILIAL-CONTRATANTE
           MOVE    WRK-COMM-ENDERECO   TO CADA-LOGRADOURO
           MOVE    WRK-COMM-NUMERO     TO CADA-NRO-LOGRADOURO

           IF  WRK-COMM-COMPL-ENDERECO EQUAL LOW-VALUES
               MOVE SPACES             TO CADA-COMPL-LOGRADOURO
           ELSE
               MOVE WRK-COMM-COMPL-ENDERECO
                                       TO CADA-COMPL-LOGRADOURO
           END-IF.

           MOVE    WRK-COMM-CIDADE     TO CADA-MUNICIPIO
           MOVE    WRK-COMM-UF         TO CADA-ESTADO
           MOVE    WRK-COMM-CEP-R      TO CADA-CEP
           STRING '000'
                   WRK-COMM-DDD-X
                   DELIMITED BY SIZE INTO CADA-DDD
           END-STRING
           MOVE    WRK-COMM-TELEFONE-X       TO CADA-TELEFONE

           MOVE    WRK-COMM-FUNCD      TO CADA-INTER-D-FUNC
           MOVE    WRK-COMM-NOMED      TO CADA-NOME-INTER-D
           MOVE    WRK-COMM-CPFD       TO WRK-CPF
           MOVE    WRK-COMM-CONTROLD   TO WRK-CONTROLE
           MOVE    WRK-CPF-AUX-R       TO CADA-CPF-INTER-D
           MOVE    WRK-COMM-DTNASCD    TO CADA-DT-NASC-INTER-D
4SYSFB     MOVE    WRK-COMM-EMAIL      TO CADA-EMAIL-INTER-D
           STRING '000'
                   WRK-COMM-DDDD-X
                   DELIMITED BY SIZE INTO CADA-DDD-INTER-D
           END-STRING
           MOVE    WRK-COMM-TELD-X      TO CADA-TELEFONE-INTER-D

           MOVE    WRK-COMM-FUNCO      TO CADA-INTER-O-FUNC
           MOVE    WRK-COMM-NOMEO      TO CADA-NOME-INTER-O
           MOVE    WRK-COMM-CPFO       TO WRK-CPF
           MOVE    WRK-COMM-CONTROLO   TO WRK-CONTROLE
           MOVE    WRK-CPF-AUX-R       TO CADA-CPF-INTER-O
           MOVE    WRK-COMM-DTNASCO    TO CADA-DT-NASC-INTER-O
4SYSFB     MOVE    WRK-COMM-EMAIL2     TO CADA-EMAIL-INTER-O
           STRING '000'
                   WRK-COMM-DDDO-X
                   DELIMITED BY SIZE INTO CADA-DDD-INTER-O
           END-STRING
           MOVE    WRK-COMM-TELO-X      TO CADA-TELEFONE-INTER-O

           MOVE    237                 TO CADA-BANCO-VENDEU
           MOVE    WRK-COMM-AG-ORIG    TO CADA-AGENCIA-VENDEU

           IF  WRK-COMM-CPF3           GREATER ZEROS
               MOVE WRK-COMM-CPF3      TO WRK-CPF
               MOVE WRK-COMM-CONTROL3  TO WRK-CONTROLE
               MOVE WRK-CPF-AUX-R      TO CADA-CPF-VENDEDOR
           ELSE
               MOVE WRK-COMM-CODFUNC   TO WRK-COD-FUNC-7
               MOVE WRK-COD-FUNC-11    TO CADA-CPF-VENDEDOR
           END-IF.

           MOVE    SPACES              TO CADA-NOME-VENDEDOR
           MOVE    ZEROS               TO CADA-DDD-VENDEDOR
                                          CADA-TELEFONE-VENDEDOR

           EXEC CICS
                ASKTIME ABSTIME (WRK-ABSTIME)
           END-EXEC

           EXEC CICS
                FORMATTIME ABSTIME  (WRK-ABSTIME)
                           DDMMYYYY (CADA-DT-ASSINATURA-CONTRATO)
           END-EXEC

4SYSFB*    DADOS DE LIMITE
  .        IF WRK-COMM-CONTAGAR        EQUAL 'X'
  .           MOVE WRK-COMM-LIMIT-CTAG TO WRK-COMM-VCONTR
  .           MOVE WRK-COMM-VALDD-CTAG TO WRK-DATA-X
  .        END-IF.
  .
  .        IF WRK-COMM-CARTAO          EQUAL 'X'
  .           MOVE WRK-COMM-LIMIT-CARTAO
  .                                    TO WRK-COMM-VCONTR
  .           MOVE WRK-COMM-VALDD-CARTAO
  .                                    TO WRK-DATA-X
  .        END-IF.
  .
  .        IF WRK-COMM-OUTROS          EQUAL 'X'
  .          IF WRK-COMM-NCON1         EQUAL '9999'
  .             MOVE WRK-COMM-LIMIT-CONE
  .                                    TO WRK-COMM-VCONTR
  .           ELSE
  .              MOVE WRK-COMM-LIMITE-TELA
  .                                    TO WRK-COMM-VCONTR
  .           END-IF
  .           MOVE WRK-COMM-VALDD-CONE TO WRK-DATA-X
  .        END-IF.
  .
  .        EXEC CICS
  .             FORMATTIME ABSTIME  (WRK-ABSTIME)
  .                        DDMMYYYY (CADA-DT-INICIO-VL-CRED)
  .        END-EXEC
  .
  .        MOVE WRK-COMM-VCONTR        TO CADA-LIMITE-CREDITO
  .        MOVE WRK-DIA-X              TO WRK-DIA-FIM
  .        MOVE WRK-MES-X              TO WRK-MES-FIM
  .        MOVE WRK-ANO-X              TO WRK-ANO-FIM
  .        MOVE WRK-DATA-FIM           TO WRK-COMM-DVCTO-LIM-ROTVO
  .        MOVE WRK-COMM-DVCTO-LIM-ROTVO
  .                                    TO CADA-DT-FIM-VL-CRED
  .        MOVE   'N'                  TO CADA-MODIFICAR-LIM-CRED
  .
4SYSFB*
           MOVE WRK-COMM-QTD-BEN       TO CADA-QTD-FUNC-BENEFI
           MOVE WRK-COMM-VL-MEDIO      TO WRK-VL-MED-AUX
           MOVE WRK-VL-MED-R           TO CADA-VLR-MEDIO-BENEFI

           IF WRK-COMM-FORMA-PG        EQUAL 'A'
              MOVE 1                   TO CADA-FORMA-PAGTO
4SYSFB        MOVE ZEROS               TO CADA-DT-INICIO-VL-CRED
4SYSFB                                    CADA-DT-FIM-VL-CRED
           ELSE
              MOVE 2                   TO CADA-FORMA-PAGTO
           END-IF.

           MOVE    WRK-COMM-MEIO-PG(3:1)  TO CADA-MEIO-PAGTO
           MOVE    WRK-COMM-AGENCIA       TO CADA-AGENCIA-DEBTO-CTA
           MOVE    WRK-COMM-CONTA         TO CADA-CONTA-DEBTO-CTA
           MOVE    WRK-COMM-DIG-CONTA     TO CADA-DIG-CONTA-DEBTO-CTA
           MOVE    WRK-COMM-NOME-GRAVACAO TO CADA-NOME-EMBOSSING
           MOVE    WRK-COMM-ENTREGA-CENTR TO CADA-ENTREGA-CENTRAL
           MOVE    WRK-COMM-FATURA-CENTR  TO CADA-FATURA-CENTRAL

           IF WRK-COMM-ENTREGA-CENTR      EQUAL 'S'
              MOVE    WRK-COMM-CNPJ       TO WRK-CNPJ
              MOVE    WRK-COMM-FILIALE    TO WRK-FILIAL
              MOVE    WRK-COMM-CONTROLE   TO WRK-CONTROLE-CNPJ
              MOVE    WRK-CNPJ-AUX-R      TO CADA-CNPJ-ENTREGA

7C2511*         MOVE    ZEROS               TO CADA-CNPJ-ENTREGA
           ELSE
7C2511          MOVE    SPACES              TO CADA-CNPJ-ENTREGA
           END-IF

           IF WRK-COMM-FATURA-CENTR       EQUAL 'S'
              MOVE    WRK-COMM-CNPJ       TO WRK-CNPJ
              MOVE    WRK-COMM-FILIALF    TO WRK-FILIAL
              MOVE    WRK-COMM-CONTROLF   TO WRK-CONTROLE-CNPJ
              MOVE    WRK-CNPJ-AUX-R      TO CADA-CNPJ-FATURA
              MOVE    WRK-COMM-FILIALC    TO WRK-FILIAL
              MOVE    WRK-COMM-CONTROLC   TO WRK-CONTROLE-CNPJ
              MOVE    WRK-CNPJ-AUX-R      TO CADA-CNPJ-COBRANCA

7C2511*         MOVE    ZEROS               TO CADA-CNPJ-FATURA
7C2511*                                        CADA-CNPJ-COBRANCA
           ELSE
7C2511          MOVE    SPACES              TO CADA-CNPJ-FATURA
7C2511                                           CADA-CNPJ-COBRANCA
           END-IF

           MOVE    WRK-COMM-REEMIS     TO CADA-FUNC-PEDIR-REEMISSAO
           MOVE    WRK-COMM-COD-AUT-X  TO CADA-COD-AUT-TARIFA
           MOVE    WRK-COMM-PRAZO-PG-X TO CADA-PRAZO-PAGAMENTO

           IF WRK-COMM-FORMA-PG         EQUAL 'A'
              MOVE  ZEROS               TO WRK-COMM-PRAZO-PG
              MOVE  WRK-COMM-PRAZO-PG-X TO CADA-PRAZO-PAGAMENTO
           END-IF

           PERFORM 0610-OBTER-TARIFAS

           MOVE   'N'                  TO CADA-MODIFICAR-INTER
           MOVE    WRK-COMM-COD-AUT-CONTA-X
                                       TO CADA-COD-AUT-CONTA

           MOVE    07                  TO WRK-LOCAL-EIBRESP
                                          WRK-LOCAL-ABCODE

           EXEC CICS
                WRITE DATASET  ('VVPEDICT')
                      FROM     (CADA-PEDIDO-CARTAO)
                      RIDFLD   (CADA-PDCT-CH)
                      NOHANDLE
           END-EXEC

           IF EIBRESP                  NOT EQUAL DFHRESP(NORMAL)
              PERFORM 9998-TRATAR-ERRO
           END-IF

4SYSFB     IF WRK-COMM-OUTROS          EQUAL 'X'
  .           PERFORM 0620-GRAVAR-SOLC-APROV
4SYSFB     END-IF

           MOVE   'VV09'               TO WRK-COMM-TRANSACAO
           MOVE    WRK-MSG17           TO WRK-COMM-MENSAG

           MOVE    08                  TO WRK-LOCAL-EIBRESP
                                          WRK-LOCAL-ABCODE

           EXEC CICS
                START TRANSID ('VV26')
                      FROM    (WRK-COMMAREA)
                      LENGTH  (LENGTH OF WRK-COMMAREA)
                      TERMID  (EIBTRMID)
           END-EXEC

           IF EIBRESP                  NOT EQUAL DFHRESP(NORMAL)
              PERFORM 9998-TRATAR-ERRO
           END-IF

           EXEC CICS
                RETURN
           END-EXEC.

      *----------------------------------------------------------------*
       0600-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       0610-OBTER-TARIFAS              SECTION.
      *----------------------------------------------------------------*

           MOVE    1                   TO WRK-IND3

           PERFORM VARYING WRK-IND     FROM 1 BY 1
                   UNTIL   WRK-IND     GREATER WRK-COMM-QTD-PAG-1805
                   PERFORM 0611-LER-TS-VV05

                   PERFORM VARYING WRK-IND2 FROM 1 BY 1
                           UNTIL   WRK-IND2 GREATER  12 OR
                                   WRK-TAB-COD-VV05-X(WRK-IND2)
                                       EQUAL ZEROS
                      MOVE WRK-TAB-COD-VV05(WRK-IND2)
                                       TO CADA-TARIFA(WRK-IND3)
                      MOVE WRK-TAB-VLTAR-VV05(WRK-IND2)
                                       TO CADA-VALOR-TARIFA(WRK-IND3)
                      ADD  1           TO WRK-IND3
                   END-PERFORM
           END-PERFORM.

           PERFORM VARYING WRK-IND3 FROM WRK-IND3 BY 1
                   UNTIL   WRK-IND3 GREATER 50
                   MOVE ZEROS          TO CADA-TARIFA(WRK-IND3)
                                          CADA-VALOR-TARIFA(WRK-IND3)
           END-PERFORM.

      *----------------------------------------------------------------*
       0610-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       0611-LER-TS-VV05                SECTION.
      *----------------------------------------------------------------*

           MOVE    09                  TO WRK-LOCAL-EIBRESP
                                          WRK-LOCAL-ABCODE

           MOVE    WRK-IND             TO WRK-ITEM-TS-VV05

           EXEC CICS
                READQ TS QUEUE (WRK-NOME-TS-VV05)
                         INTO  (WRK-AREA-TS-VV05)
                         ITEM  (WRK-ITEM-TS-VV05)
           END-EXEC.

           IF EIBRESP                  NOT EQUAL DFHRESP(NORMAL)
              PERFORM 9998-TRATAR-ERRO
           END-IF.

      *----------------------------------------------------------------*
       0611-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

4SYSFB*----------------------------------------------------------------*
  .    0620-GRAVAR-SOLC-APROV          SECTION.
  .   *----------------------------------------------------------------*
  .
  .        MOVE 10                     TO WRK-LOCAL-EIBRESP
  .                                       WRK-LOCAL-ABCODE
  .
  .        IF WRK-COMM-AG-ORIG         NOT NUMERIC
  .           MOVE ZEROS               TO SPARM-LIM-CH-AGENCIA4
  .        ELSE
  .           MOVE WRK-COMM-AG-ORIG    TO SPARM-LIM-CH-AGENCIA4
  .        END-IF.
  .
  .        MOVE ZEROS                  TO SPARM-LIM-CH-DIGITO.
  .        MOVE WRK-COMM-CNPJ          TO SPARM-LIM-CH-CNPJ-CONTR.
  .        MOVE WRK-COMM-FILIAL        TO SPARM-LIM-CH-FILIAL-CONTR.
  .        MOVE WRK-COMM-CONTROL       TO SPARM-LIM-CH-CTRL-CONTR.
  .        MOVE WRK-COMM-PROD          TO SPARM-LIM-CH-PRODUTO.
  .        MOVE SPACES                 TO SPARM-LIM-FILLER.
  .        MOVE WRK-COMM-NOME-EMP      TO SPARM-LIM-RAZAO-SOCIAL.
  .
  .        IF WRK-COMM-NCON1           EQUAL '9999'
  .           MOVE WRK-COMM-LIMIT-CONE
  .                                    TO SPARM-LIM-CREDITO
  .        ELSE
  .           MOVE WRK-COMM-LIMITE-TELA
  .                                    TO SPARM-LIM-CREDITO
  .        END-IF
  .
  .        MOVE WRK-COMM-VALDD-CONE    TO WRK-DATA-X.
  .        MOVE WRK-DIA-X              TO WRK-DIA-AUX-INV.
  .        MOVE WRK-MES-X              TO WRK-MES-AUX-INV.
  .        MOVE WRK-ANO-X              TO WRK-ANO-AUX-INV.
  .        MOVE WRK-DATA-AUX-INV-R     TO SPARM-LIM-DT-FIM-VL-CRED.
  .
  .        EXEC CICS
  .             FORMATTIME ABSTIME  (WRK-ABSTIME)
  .                        YYYYMMDD (SPARM-LIM-DT-INCLUSAO)
  .        END-EXEC
  .
  .        EXEC CICS
  .             ASSIGN USERID (WRK-USUARIO)
  .        END-EXEC.
  .
  .        MOVE WRK-USUARIO            TO SPARM-LIM-USUARIO-INCL.
  .        MOVE 'P'                    TO SPARM-LIM-STATUS.
  .        MOVE ZEROS                  TO SPARM-LIM-DT-PARE-REG
  .                                       SPARM-LIM-DT-PARE-BCK
  .        MOVE SPACES                 TO SPARM-LIM-PARECER-REG
  .                                       SPARM-LIM-PARECER-BCK
  .                                       SPARM-LIM-USUARIO-REG
  .                                       SPARM-LIM-USUARIO-BCK
  .                                       SPARM-LIM-FILLER-02
  .
  .        EXEC CICS
  .             WRITE DATASET  ('SPLIMTEC')
  .                   FROM     (SPARM-LIM-TECNICO)
  .                   RIDFLD   (SPARM-LIM-CH)
  .                   NOHANDLE
  .        END-EXEC.
  .
  .        IF EIBRESP                  NOT EQUAL DFHRESP(NORMAL)
  .           PERFORM 9998-TRATAR-ERRO
  .        END-IF.
  .
  .   *----------------------------------------------------------------*
  .    0620-99-FIM.                    EXIT.
4SYSFB*----------------------------------------------------------------*

      *----------------------------------------------------------------*
       0700-TRATAR-PF9                 SECTION.
      *----------------------------------------------------------------*

           MOVE    LOW-VALUES          TO NOMEGRI
                                          ENTREGAI
                                          FILIALEI
                                          CONTREI
                                          FILIALFI
                                          CONTRFI
                                          FATURAI
                                          FILIALCI
                                          CONTRCI
                                          FUNCDI
                                          CPFDI
                                          CONTRDI
                                          DDNASCDI
                                          MMNASCDI
                                          AANASCDI
                                          NOMEDI
                                          DDDDI
                                          TELDI
                                          MESMOSI
                                          FUNCOI
                                          CPFOI
                                          CONTROPI
                                          DDNASCOI
                                          MMNASCOI
                                          AANASCOI
                                          NOMEOI
                                          DDDOI
                                          TELOI
                                          CPF3I
                                          CONTR3I
                                          CODFUNCI
4SYSFB                                    EMAILI
4SYSFB                                    EMAIL2I.

           MOVE   '1'                  TO WRK-COMM-FASE
           MOVE   -1                   TO NOMEGRL
           MOVE    WRK-MSG03           TO MENSAGI

           PERFORM 0410-ENVIAR-MAPA.

      *----------------------------------------------------------------*
       0700-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       0800-TRATAR-ENTER               SECTION.
      *----------------------------------------------------------------*

           EVALUATE WRK-COMM-FASE
               WHEN '1'
                    PERFORM 0810-TRATAR-FASE-1

               WHEN '2'
                    PERFORM 0820-TRATAR-FASE-2
           END-EVALUATE.

      *----------------------------------------------------------------*
       0800-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       0810-TRATAR-FASE-1              SECTION.
      *----------------------------------------------------------------*

           IF NOMEGRI                  EQUAL SPACES OR LOW-VALUES
              MOVE   -1                TO NOMEGRL
              MOVE   'I'               TO NOMEGRA
              MOVE    WRK-MSG04        TO MENSAGO
              PERFORM 0410-ENVIAR-MAPA
           END-IF

           IF ENTREGAI                 NOT EQUAL 'S' AND 'N'
              MOVE   -1                TO ENTREGAL
              MOVE   'I'               TO ENTREGAA
              MOVE    WRK-MSG05        TO MENSAGO
              PERFORM 0410-ENVIAR-MAPA
           END-IF

7C2511*        IF FILIALEI              IS NOT NUMERIC OR
7C2511*           FILIALEI              EQUAL ZEROS
           IF (ENTREGAI                 EQUAL 'S')
              IF (FILIALEI              EQUAL SPACES) OR
                 (FILIALEI              EQUAL LOW-VALUES)
                 MOVE   -1             TO FILIALEL
                 MOVE   'R'            TO FILIALEA
                 MOVE    WRK-MSG06     TO MENSAGO
                 PERFORM 0410-ENVIAR-MAPA
              END-IF

              IF CONTREI               IS NOT NUMERIC
                 MOVE   -1             TO CONTREL
                 MOVE   'R'            TO CONTREA
                 MOVE    WRK-MSG07     TO MENSAGO
                 PERFORM 0410-ENVIAR-MAPA
              END-IF

              PERFORM 0811-VALIDAR-CNPJ-ENTREGA

           ELSE
              IF (FILIALEI             NOT EQUAL SPACES AND LOW-VALUES)
              OR (CONTREI              NOT EQUAL SPACES AND LOW-VALUES)
                 MOVE   -1             TO FILIALEL
                 MOVE   'R'            TO FILIALEA
                                          CONTREA
                 MOVE    WRK-MSG09     TO MENSAGO
                 PERFORM 0410-ENVIAR-MAPA
              END-IF
           END-IF

           IF FATURAI                  NOT EQUAL 'S' AND 'N'
              MOVE   -1                TO FATURAL
              MOVE   'I'               TO FATURAA
              MOVE    WRK-MSG05        TO MENSAGO
              PERFORM 0410-ENVIAR-MAPA
           END-IF

7C2511*        IF FILIALFI              IS NOT NUMERIC OR
7C2511*           FILIALFI              EQUAL ZEROS
           IF (FATURAI                  EQUAL 'S')
              IF (FILIALFI              EQUAL SPACES) OR
                 (FILIALFI              EQUAL LOW-VALUES)
                 MOVE   -1             TO FILIALFL
                 MOVE   'R'            TO FILIALFA
                 MOVE    WRK-MSG06     TO MENSAGO
                 PERFORM 0410-ENVIAR-MAPA
              END-IF

              IF CONTRFI               IS NOT NUMERIC
                 MOVE   -1             TO CONTRFL
                 MOVE   'R'            TO CONTRFA
                 MOVE    WRK-MSG07     TO MENSAGO
                 PERFORM 0410-ENVIAR-MAPA
              END-IF

              PERFORM 0812-VALIDAR-CNPJ-FATURAMENTO

           ELSE
              IF (FILIALFI             NOT EQUAL SPACES AND LOW-VALUES)
              OR (CONTRFI              NOT EQUAL SPACES AND LOW-VALUES)
                 MOVE   -1             TO FILIALFL
                 MOVE   'R'            TO FILIALFA
                                          CONTRFA
                 MOVE    WRK-MSG10     TO MENSAGO
                 PERFORM 0410-ENVIAR-MAPA
              END-IF

              IF (FILIALCI             NOT EQUAL SPACES AND LOW-VALUES)
              OR (CONTRCI              NOT EQUAL SPACES AND LOW-VALUES)
                 MOVE   -1             TO FILIALCL
                 MOVE   'R'            TO FILIALCA
                                          CONTRCA
                 MOVE    WRK-MSG10     TO MENSAGO
                 PERFORM 0410-ENVIAR-MAPA
              END-IF
           END-IF

7C2511*        IF FILIALCI              IS NOT NUMERIC OR
7C2511*           FILIALCI              EQUAL ZEROS
           IF (FATURAI                  EQUAL 'S')
              IF (FILIALCI              EQUAL SPACES) OR
                 (FILIALCI              EQUAL LOW-VALUES)
                 MOVE    FILIALFI      TO FILIALCI
              END-IF

              IF CONTRCI               IS NOT NUMERIC
                 MOVE    CONTRFI       TO CONTRCI
              END-IF

              PERFORM 0813-VALIDAR-CNPJ-COBRANCA
           END-IF

           IF FUNCDI                   NOT EQUAL 'S' AND 'N'
              MOVE   -1                TO FUNCDL
              MOVE   'I'               TO FUNCDA
              MOVE    WRK-MSG05        TO MENSAGO
              PERFORM 0410-ENVIAR-MAPA
           END-IF

           IF CPFDI                    IS NOT NUMERIC OR
              CPFDI                    EQUAL ZEROS
              MOVE   -1                TO CPFDL
              MOVE   'R'               TO CPFDA
              MOVE    WRK-MSG11        TO MENSAGO
              PERFORM 0410-ENVIAR-MAPA
           END-IF

           IF CONTRDI                  IS NOT NUMERIC
              MOVE   -1                TO CONTRDL
              MOVE   'R'               TO CONTRDA
              MOVE    WRK-MSG08        TO MENSAGO
              PERFORM 0410-ENVIAR-MAPA
           END-IF

           PERFORM 0814-VALIDAR-CPF-DECISAO

           IF DDNASCDI                 IS NOT NUMERIC OR
              DDNASCDI                 EQUAL ZEROS
              MOVE   -1                TO DDNASCDL
              MOVE   'R'               TO DDNASCDA
                                          MMNASCDA
                                          AANASCDA
              MOVE    WRK-MSG12        TO MENSAGO
              PERFORM 0410-ENVIAR-MAPA
           END-IF

           IF MMNASCDI                 IS NOT NUMERIC OR
              MMNASCDI                 EQUAL ZEROS
              MOVE   -1                TO MMNASCDL
              MOVE   'R'               TO DDNASCDA
                                          MMNASCDA
                                          AANASCDA
              MOVE    WRK-MSG12        TO MENSAGO
              PERFORM 0410-ENVIAR-MAPA
           END-IF

           IF AANASCDI                 IS NOT NUMERIC OR
              AANASCDI                 EQUAL ZEROS
              MOVE   -1                TO AANASCDL
              MOVE   'R'               TO DDNASCDA
                                          MMNASCDA
                                          AANASCDA
              MOVE    WRK-MSG12        TO MENSAGO
              PERFORM 0410-ENVIAR-MAPA
           END-IF

           MOVE    DDNASCDI            TO WRK-DIA-AUX-INV-X
           MOVE    MMNASCDI            TO WRK-MES-AUX-INV-X
           MOVE    AANASCDI            TO WRK-ANO-AUX-INV-X
           PERFORM 0815-CONSISTIR-DATA

           IF WRK-OPCAO                EQUAL 'I'
              MOVE   -1                TO DDNASCDL
              MOVE   'R'               TO DDNASCDA
                                          MMNASCDA
                                          AANASCDA
              MOVE    WRK-MSG12        TO MENSAGO
              PERFORM 0410-ENVIAR-MAPA
           END-IF

           IF NOMEDI                   EQUAL SPACES OR LOW-VALUES
              MOVE   -1                TO NOMEDL
              MOVE   'I'               TO NOMEDA
              MOVE    WRK-MSG13        TO MENSAGO
              PERFORM 0410-ENVIAR-MAPA
           END-IF

           IF DDDDI                    EQUAL SPACES OR LOW-VALUES
              MOVE   -1                TO DDDDL
              MOVE   'R'               TO DDDDA
              MOVE    WRK-MSG14        TO MENSAGO
              PERFORM 0410-ENVIAR-MAPA

           ELSE
              MOVE DDDDI               TO WRK-DDD-BIF
              EXEC CICS
                   BIF DEEDIT FIELD(WRK-DDD-BIF)
              END-EXEC

              IF WRK-DDD-BIF           NOT NUMERIC OR
                 WRK-DDD-BIF-R         EQUAL ZEROS
                 MOVE   -1             TO DDDDL
                 MOVE   'R'            TO DDDDA
                 MOVE    WRK-MSG14     TO MENSAGO
                 PERFORM 0410-ENVIAR-MAPA
              ELSE
                 MOVE    WRK-DDD-BIF-R TO DDDDO
              END-IF
           END-IF

           IF TELDI                    IS NOT NUMERIC OR
              TELDI                    EQUAL ZEROS
              MOVE   -1                TO TELDL
              MOVE   'R'               TO TELDA
              MOVE    WRK-MSG15        TO MENSAGO
              PERFORM 0410-ENVIAR-MAPA
           END-IF

4SYSFB     IF EMAILI                   EQUAL SPACES OR LOW-VALUES
.             MOVE   -1                TO EMAILL
.             MOVE   'I'               TO EMAILA
.             MOVE   WRK-MSG21         TO MENSAGO
.             PERFORM 0410-ENVIAR-MAPA
.          ELSE
.             MOVE EMAILI              TO WRK-EMAIL
.             PERFORM 0819-CONSISTE-EMAIL
.             IF WRK-VALIDAR-EMAIL     EQUAL 'S'
.                MOVE   -1             TO EMAILL
.                MOVE   'I'            TO EMAILA
.                PERFORM 0410-ENVIAR-MAPA
.             END-IF
4SYSFB     END-IF.

           IF MESMOSI                  NOT EQUAL 'S' AND 'N'
              MOVE   -1                TO MESMOSL
              MOVE   'I'               TO MESMOSA
              MOVE    WRK-MSG05        TO MENSAGO
              PERFORM 0410-ENVIAR-MAPA
           END-IF

           IF MESMOSI                  EQUAL 'N'
              IF FUNCOI                NOT EQUAL 'S' AND 'N'
                 MOVE   -1             TO FUNCOL
                 MOVE   'I'            TO FUNCOA
                 MOVE    WRK-MSG05     TO MENSAGO
                 PERFORM 0410-ENVIAR-MAPA
              END-IF

              IF CPFOI                 IS NOT NUMERIC OR
                 CPFOI                 EQUAL ZEROS
                 MOVE   -1             TO CPFOL
                 MOVE   'R'            TO CPFOA
                 MOVE    WRK-MSG11     TO MENSAGO
                 PERFORM 0410-ENVIAR-MAPA
              END-IF

              IF CONTROPI              IS NOT NUMERIC
                 MOVE   -1             TO CONTROPL
                 MOVE   'R'            TO CONTROPA
                 MOVE    WRK-MSG08     TO MENSAGO
                 PERFORM 0410-ENVIAR-MAPA
              END-IF

              PERFORM 0816-VALIDAR-CPF-OPERACAO

              IF DDNASCOI              IS NOT NUMERIC OR
                 DDNASCOI              EQUAL ZEROS
                 MOVE   -1             TO DDNASCOL
                 MOVE   'R'            TO DDNASCOA
                                          MMNASCOA
                                          AANASCOA
                 MOVE    WRK-MSG12     TO MENSAGO
                 PERFORM 0410-ENVIAR-MAPA
              END-IF

              IF MMNASCOI              IS NOT NUMERIC OR
                 MMNASCOI              EQUAL ZEROS
                 MOVE   -1             TO MMNASCOL
                 MOVE   'R'            TO DDNASCOA
                                          MMNASCOA
                                          AANASCOA
                 MOVE    WRK-MSG12     TO MENSAGO
                 PERFORM 0410-ENVIAR-MAPA
              END-IF

              IF AANASCOI              IS NOT NUMERIC OR
                 AANASCOI              EQUAL ZEROS
                 MOVE   -1             TO AANASCOL
                 MOVE   'R'            TO DDNASCOA
                                          MMNASCOA
                                          AANASCOA
                 MOVE    WRK-MSG12     TO MENSAGO
                 PERFORM 0410-ENVIAR-MAPA
              END-IF

              MOVE    DDNASCOI         TO WRK-DIA-AUX-INV-X
              MOVE    MMNASCOI         TO WRK-MES-AUX-INV-X
              MOVE    AANASCOI         TO WRK-ANO-AUX-INV-X
              PERFORM 0815-CONSISTIR-DATA

              IF WRK-OPCAO             EQUAL 'I'
                 MOVE   -1             TO DDNASCOL
                 MOVE   'R'            TO DDNASCOA
                                          MMNASCOA
                                          AANASCOA
                 MOVE    WRK-MSG12     TO MENSAGO
                 PERFORM 0410-ENVIAR-MAPA
              END-IF

              IF NOMEOI                EQUAL SPACES OR LOW-VALUES
                 MOVE   -1             TO NOMEOL
                 MOVE   'I'            TO NOMEOA
                 MOVE    WRK-MSG13     TO MENSAGO
                 PERFORM 0410-ENVIAR-MAPA
              END-IF

              IF DDDOI                 EQUAL SPACES OR LOW-VALUES
                 MOVE   -1             TO DDDOL
                 MOVE   'R'            TO DDDOA
                 MOVE    WRK-MSG14     TO MENSAGO
                 PERFORM 0410-ENVIAR-MAPA

              ELSE
                 MOVE DDDOI               TO WRK-DDD-BIF
                 EXEC CICS
                      BIF DEEDIT FIELD(WRK-DDD-BIF)
                 END-EXEC

                 IF WRK-DDD-BIF        NOT NUMERIC OR
                    WRK-DDD-BIF-R      EQUAL ZEROS
                    MOVE   -1          TO DDDOL
                    MOVE   'R'         TO DDDOA
                    MOVE    WRK-MSG14  TO MENSAGO
                    PERFORM 0410-ENVIAR-MAPA

                 ELSE
                    MOVE    WRK-DDD-BIF-R
                                       TO DDDOO
                 END-IF
              END-IF

              IF TELOI                 IS NOT NUMERIC OR
                 TELOI                 EQUAL ZEROS
                 MOVE   -1             TO TELOL
                 MOVE   'R'            TO TELOA
                 MOVE    WRK-MSG15     TO MENSAGO
                 PERFORM 0410-ENVIAR-MAPA
              END-IF

4SYSFB        IF EMAIL2I               EQUAL SPACES OR LOW-VALUES
.                MOVE   -1             TO EMAIL2L
.                MOVE   'I'            TO EMAIL2A
.                MOVE   WRK-MSG21      TO MENSAGO
.                PERFORM 0410-ENVIAR-MAPA
.             ELSE
.                MOVE EMAIL2I          TO WRK-EMAIL
.                PERFORM 0819-CONSISTE-EMAIL
.                IF WRK-VALIDAR-EMAIL  EQUAL 'S'
.                   MOVE   -1          TO EMAIL2L
.                   MOVE   'I'         TO EMAIL2A
.                   PERFORM 0410-ENVIAR-MAPA
.                END-IF
4SYSFB        END-IF

           ELSE
              MOVE    FUNCDI           TO FUNCOI
              MOVE    CPFDI            TO CPFOI
              MOVE    CONTRDI          TO CONTROPI
              MOVE    DDNASCDI         TO DDNASCOI
              MOVE    MMNASCDI         TO MMNASCOI
              MOVE    AANASCDI         TO AANASCOI
              MOVE    NOMEDI           TO NOMEOI
              MOVE    DDDDI            TO DDDOI
              MOVE    TELDI            TO TELOI
4SYSFB        MOVE    EMAILI           TO EMAIL2I
           END-IF

           MOVE    NOMEGRO             TO WRK-COMM-NOME-GRAVACAO
           MOVE    ENTREGAO            TO WRK-COMM-ENTREGA-CENTR

           IF ENTREGAI                 EQUAL 'S'
              MOVE    FILIALEO         TO WRK-COMM-FILIALE
              MOVE    CONTREO          TO WRK-COMM-CONTROLE

7C2511*         MOVE    ZEROS            TO WRK-COMM-FILIALE
           ELSE
7C2511          MOVE    SPACES           TO WRK-COMM-FILIALE
                                          WRK-COMM-CONTROLE
           END-IF

           MOVE    FATURAO             TO WRK-COMM-FATURA-CENTR

           IF FATURAI                  EQUAL 'S'
              MOVE    FILIALFO         TO WRK-COMM-FILIALF
              MOVE    CONTRFO          TO WRK-COMM-CONTROLF
              MOVE    FILIALCO         TO WRK-COMM-FILIALC
              MOVE    CONTRCO          TO WRK-COMM-CONTROLC

7C2511*         MOVE    ZEROS            TO WRK-COMM-FILIALF
7C2511*                                     WRK-COMM-CONTROLF
7C2511*                                     WRK-COMM-FILIALC
7C2511*                                     WRK-COMM-CONTROLC
           ELSE
7C2511          MOVE    SPACES           TO WRK-COMM-FILIALF
                                          WRK-COMM-CONTROLF
                                          WRK-COMM-FILIALC
                                          WRK-COMM-CONTROLC
           END-IF

           IF CPF3I                    EQUAL SPACES OR LOW-VALUES
              NEXT SENTENCE
           ELSE
              IF CPF3I                 IS NOT NUMERIC
                 MOVE   -1             TO CPF3L
                 MOVE   'R'            TO CPF3A
                 MOVE    WRK-MSG18     TO MENSAGO
                 PERFORM 0410-ENVIAR-MAPA
              END-IF
           END-IF.

           IF CONTR3I                  EQUAL SPACES OR LOW-VALUES
              NEXT SENTENCE
           ELSE
              IF CONTR3I               IS NOT NUMERIC
                 MOVE   -1             TO CONTR3L
                 MOVE   'R'            TO CONTR3A
                 MOVE    WRK-MSG08     TO MENSAGO
                 PERFORM 0410-ENVIAR-MAPA
              END-IF
           END-IF.

           IF CPF3I                    IS NUMERIC AND
              CPF3I                    GREATER ZEROS
               PERFORM 0818-VALIDAR-CPF-VENDEDOR
           END-IF.

           IF CODFUNCI                 EQUAL SPACES OR LOW-VALUES
              NEXT SENTENCE
           ELSE
              IF CODFUNCI              IS NOT NUMERIC
                 MOVE   -1             TO CODFUNCL
                 MOVE   'R'            TO CODFUNCA
                 MOVE    WRK-MSG19     TO MENSAGO
                 PERFORM 0410-ENVIAR-MAPA
              END-IF
           END-IF.

           IF ((CPF3I                  EQUAL SPACES     OR
                CPF3I                  EQUAL LOW-VALUES OR
                CPF3I                  EQUAL ZEROS)
              AND
               (CODFUNCI               EQUAL SPACES     OR
                CODFUNCI               EQUAL LOW-VALUES OR
                CODFUNCI               EQUAL ZEROS))
                MOVE   -1              TO CPF3L
                MOVE   'R'             TO CPF3A
                                          CONTR3A
                                          CODFUNCA
                MOVE    WRK-MSG20      TO MENSAGO
                PERFORM 0410-ENVIAR-MAPA
           END-IF.

           IF CPF3I                    GREATER ZEROS AND
              CODFUNCI                 GREATER ZEROS
                MOVE   -1              TO CPF3L
                MOVE   'R'             TO CPF3A
                                          CONTR3A
                                          CODFUNCA
                MOVE    WRK-MSG20      TO MENSAGO
                PERFORM 0410-ENVIAR-MAPA
           END-IF.

           MOVE    FUNCDO              TO WRK-COMM-FUNCD
           MOVE    CPFDO               TO WRK-COMM-CPFD
           MOVE    CONTRDO             TO WRK-COMM-CONTROLD
           MOVE    DDNASCDO            TO WRK-DIA-AUX
           MOVE    MMNASCDO            TO WRK-MES-AUX
           MOVE    AANASCDO            TO WRK-ANO-AUX
           MOVE    WRK-DATA-AUX-R      TO WRK-COMM-DTNASCD
           MOVE    NOMEDO              TO WRK-COMM-NOMED
           MOVE    DDDDO               TO WRK-COMM-DDDD
           MOVE    TELDO               TO WRK-COMM-TELD
           MOVE    MESMOSO             TO WRK-COMM-MESMOS
           MOVE    FUNCOO              TO WRK-COMM-FUNCO
           MOVE    CPFOO               TO WRK-COMM-CPFO
           MOVE    CONTROPO            TO WRK-COMM-CONTROLO
           MOVE    DDNASCOO            TO WRK-DIA-AUX
           MOVE    MMNASCOO            TO WRK-MES-AUX
           MOVE    AANASCOO            TO WRK-ANO-AUX
           MOVE    WRK-DATA-AUX-R      TO WRK-COMM-DTNASCO
           MOVE    NOMEOO              TO WRK-COMM-NOMEO
           MOVE    DDDOO               TO WRK-COMM-DDDO
           MOVE    TELOO               TO WRK-COMM-TELO
           MOVE    CPF3O               TO WRK-COMM-CPF3
           MOVE    CONTR3O             TO WRK-COMM-CONTROL3
           MOVE    CODFUNCO            TO WRK-COMM-CODFUNC
4SYSFB     MOVE    EMAILO              TO WRK-COMM-EMAIL
4SYSFB     MOVE    EMAIL2O             TO WRK-COMM-EMAIL2

           PERFORM 0817-PROTEGER-CAMPOS

           MOVE   '2'                  TO WRK-COMM-FASE
           MOVE    WRK-MSG16           TO MENSAGO
           PERFORM 0410-ENVIAR-MAPA.

      *----------------------------------------------------------------*
       0810-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       0811-VALIDAR-CNPJ-ENTREGA       SECTION.
      *----------------------------------------------------------------*

           INITIALIZE WRK-AREA-POOL1911
           STRING '0' CNPJI
                   DELIMITED BY SIZE INTO WRK-NCNPJ-CALC1
           END-STRING
           MOVE    FILIALEI            TO WRK-FILIAL-CALC1
           MOVE    WRK-CNPJ-CALCULAR1  TO WRK-POOL1911-VALOR (1:13)
           MOVE    13                  TO WRK-POOL1911-TAMANHO

           EXEC CICS
                LINK PROGRAM  ('POOL1911')
                     COMMAREA (WRK-AREA-POOL1911)
                     LENGTH   (LENGTH OF WRK-AREA-POOL1911)
           END-EXEC

           IF WRK-POOL1911-RETORNO     NOT EQUAL ZEROS OR
              EIBRESP                  NOT EQUAL DFHRESP(NORMAL)
              MOVE   '*'               TO WRK-INCONSISTENTE
              MOVE   -1                TO CONTREL
              MOVE   'R'               TO CONTREA
              MOVE    WRK-MSG08        TO MENSAGO
              PERFORM 0410-ENVIAR-MAPA

           ELSE
              IF WRK-POOL1911-DIGITO   EQUAL 'P'
                 MOVE    ZEROS         TO WRK-POOL1911-DIGITO
              END-IF

              MOVE    WRK-POOL1911-DIGITO
                                       TO WRK-DIG-RET1
                                          WRK-RETORNADO

              INITIALIZE WRK-AREA-POOL1911
              STRING '0' CNPJI
                      DELIMITED        BY SIZE
                                     INTO WRK-NCNPJ-CALC2
              END-STRING
              MOVE    FILIALEI         TO WRK-FILIAL-CALC2
              MOVE    WRK-CNPJ-CALCULAR2
                                       TO WRK-POOL1911-VALOR (1:14)
              MOVE    14               TO WRK-POOL1911-TAMANHO

              EXEC CICS
                   LINK PROGRAM  ('POOL1911')
                   COMMAREA (WRK-AREA-POOL1911)
                   LENGTH   (LENGTH OF WRK-AREA-POOL1911)
              END-EXEC

              IF WRK-POOL1911-RETORNO  EQUAL ZEROS
                 IF WRK-POOL1911-DIGITO
                                       EQUAL 'P'
                    MOVE    ZEROS      TO WRK-POOL1911-DIGITO
                 END-IF

                 MOVE    WRK-POOL1911-DIGITO
                                       TO WRK-DIG-RET2
                 MOVE    WRK-DIGITO-RETORNADO
                                       TO WRK-DIGITO-AUX
              END-IF

              IF  WRK-POOL1911-RETORNO NOT EQUAL ZEROS OR
                  WRK-DIGITO-AUX       NOT EQUAL CONTREI OR
                  EIBRESP              NOT EQUAL DFHRESP(NORMAL)
                  MOVE   '*'           TO WRK-INCONSISTENTE
                  MOVE   -1            TO CONTREL
                  MOVE   'R'           TO CONTREA
                  MOVE    WRK-MSG08    TO MENSAGO
                  PERFORM 0410-ENVIAR-MAPA
              END-IF
           END-IF.

      *----------------------------------------------------------------*
       0811-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       0812-VALIDAR-CNPJ-FATURAMENTO   SECTION.
      *----------------------------------------------------------------*

           INITIALIZE WRK-AREA-POOL1911
           STRING '0' CNPJI
                   DELIMITED BY SIZE INTO WRK-NCNPJ-CALC1
           END-STRING
           MOVE    FILIALFI            TO WRK-FILIAL-CALC1
           MOVE    WRK-CNPJ-CALCULAR1  TO WRK-POOL1911-VALOR (1:13)
           MOVE    13                  TO WRK-POOL1911-TAMANHO

           EXEC CICS
                LINK PROGRAM  ('POOL1911')
                     COMMAREA (WRK-AREA-POOL1911)
                     LENGTH   (LENGTH OF WRK-AREA-POOL1911)
           END-EXEC

           IF WRK-POOL1911-RETORNO     NOT EQUAL ZEROS OR
              EIBRESP                  NOT EQUAL DFHRESP(NORMAL)
              MOVE   '*'               TO WRK-INCONSISTENTE
              MOVE   -1                TO CONTRFL
              MOVE   'R'               TO CONTRFA
              MOVE    WRK-MSG08        TO MENSAGO
              PERFORM 0410-ENVIAR-MAPA

           ELSE
              IF WRK-POOL1911-DIGITO   EQUAL 'P'
                 MOVE    ZEROS         TO WRK-POOL1911-DIGITO
              END-IF

              MOVE    WRK-POOL1911-DIGITO
                                       TO WRK-DIG-RET1
                                          WRK-RETORNADO

              INITIALIZE WRK-AREA-POOL1911
              STRING '0' CNPJI
                      DELIMITED        BY SIZE
                                     INTO WRK-NCNPJ-CALC2
              END-STRING
              MOVE    FILIALFI         TO WRK-FILIAL-CALC2
              MOVE    WRK-CNPJ-CALCULAR2
                                       TO WRK-POOL1911-VALOR (1:14)
              MOVE    14               TO WRK-POOL1911-TAMANHO

              EXEC CICS
                   LINK PROGRAM  ('POOL1911')
                   COMMAREA (WRK-AREA-POOL1911)
                   LENGTH   (LENGTH OF WRK-AREA-POOL1911)
              END-EXEC

              IF WRK-POOL1911-RETORNO  EQUAL ZEROS
                 IF WRK-POOL1911-DIGITO
                                       EQUAL 'P'
                    MOVE    ZEROS      TO WRK-POOL1911-DIGITO
                 END-IF

                 MOVE    WRK-POOL1911-DIGITO
                                       TO WRK-DIG-RET2
                 MOVE    WRK-DIGITO-RETORNADO
                                       TO WRK-DIGITO-AUX
              END-IF

              IF  WRK-POOL1911-RETORNO NOT EQUAL ZEROS OR
                  WRK-DIGITO-AUX       NOT EQUAL CONTRFI OR
                  EIBRESP              NOT EQUAL DFHRESP(NORMAL)
                  MOVE   '*'           TO WRK-INCONSISTENTE
                  MOVE   -1            TO CONTRFL
                  MOVE   'R'           TO CONTRFA
                  MOVE    WRK-MSG08    TO MENSAGO
                  PERFORM 0410-ENVIAR-MAPA
              END-IF
           END-IF.

      *----------------------------------------------------------------*
       0812-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       0813-VALIDAR-CNPJ-COBRANCA      SECTION.
      *----------------------------------------------------------------*

           INITIALIZE WRK-AREA-POOL1911
           STRING '0' CNPJI
                   DELIMITED BY SIZE INTO WRK-NCNPJ-CALC1
           END-STRING
           MOVE    FILIALCI            TO WRK-FILIAL-CALC1
           MOVE    WRK-CNPJ-CALCULAR1  TO WRK-POOL1911-VALOR (1:13)
           MOVE    13                  TO WRK-POOL1911-TAMANHO

           EXEC CICS
                LINK PROGRAM  ('POOL1911')
                     COMMAREA (WRK-AREA-POOL1911)
                     LENGTH   (LENGTH OF WRK-AREA-POOL1911)
           END-EXEC

           IF WRK-POOL1911-RETORNO     NOT EQUAL ZEROS OR
              EIBRESP                  NOT EQUAL DFHRESP(NORMAL)
              MOVE   '*'               TO WRK-INCONSISTENTE
              MOVE   -1                TO CONTRCL
              MOVE   'R'               TO CONTRCA
              MOVE    WRK-MSG08        TO MENSAGO
              PERFORM 0410-ENVIAR-MAPA

           ELSE
              IF WRK-POOL1911-DIGITO   EQUAL 'P'
                 MOVE    ZEROS         TO WRK-POOL1911-DIGITO
              END-IF

              MOVE    WRK-POOL1911-DIGITO
                                       TO WRK-DIG-RET1
                                          WRK-RETORNADO

              INITIALIZE WRK-AREA-POOL1911
              STRING '0' CNPJI
                      DELIMITED        BY SIZE
                                     INTO WRK-NCNPJ-CALC2
              END-STRING
              MOVE    FILIALCI         TO WRK-FILIAL-CALC2
              MOVE    WRK-CNPJ-CALCULAR2
                                       TO WRK-POOL1911-VALOR (1:14)
              MOVE    14               TO WRK-POOL1911-TAMANHO

              EXEC CICS
                   LINK PROGRAM  ('POOL1911')
                   COMMAREA (WRK-AREA-POOL1911)
                   LENGTH   (LENGTH OF WRK-AREA-POOL1911)
              END-EXEC

              IF WRK-POOL1911-RETORNO  EQUAL ZEROS
                 IF WRK-POOL1911-DIGITO
                                       EQUAL 'P'
                    MOVE    ZEROS      TO WRK-POOL1911-DIGITO
                 END-IF

                 MOVE    WRK-POOL1911-DIGITO
                                       TO WRK-DIG-RET2
                 MOVE    WRK-DIGITO-RETORNADO
                                       TO WRK-DIGITO-AUX
              END-IF

              IF  WRK-POOL1911-RETORNO NOT EQUAL ZEROS OR
                  WRK-DIGITO-AUX       NOT EQUAL CONTRCI OR
                  EIBRESP              NOT EQUAL DFHRESP(NORMAL)
                  MOVE   '*'           TO WRK-INCONSISTENTE
                  MOVE   -1            TO CONTRCL
                  MOVE   'R'           TO CONTRCA
                  MOVE    WRK-MSG08    TO MENSAGO
                  PERFORM 0410-ENVIAR-MAPA
              END-IF
           END-IF.

      *----------------------------------------------------------------*
       0813-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       0814-VALIDAR-CPF-DECISAO        SECTION.
      *----------------------------------------------------------------*

           INITIALIZE WRK-AREA-POOL1911
           MOVE    CPFDI               TO WRK-POOL1911-VALOR (1:9)
           MOVE    09                  TO WRK-POOL1911-TAMANHO
           MOVE   'A'                  TO WRK-POOL1911-DIGITO

           EXEC CICS
                LINK PROGRAM  ('POOL1911')
                     COMMAREA (WRK-AREA-POOL1911)
                     LENGTH   (LENGTH OF WRK-AREA-POOL1911)
           END-EXEC

           IF WRK-POOL1911-RETORNO     NOT EQUAL ZEROS OR
              EIBRESP                  NOT EQUAL DFHRESP(NORMAL)
              MOVE   '*'               TO WRK-INCONSISTENTE
              MOVE   -1                TO CONTRDL
              MOVE   'R'               TO CONTRDA
              MOVE    WRK-MSG08        TO MENSAGO
              PERFORM 0410-ENVIAR-MAPA

           ELSE
              IF WRK-POOL1911-DIGITO   EQUAL 'P'
                 MOVE    ZEROS         TO WRK-POOL1911-DIGITO
              END-IF

              MOVE    WRK-POOL1911-DIGITO
                                       TO WRK-DIG-RET1
                                          WRK-RETORNADO

              INITIALIZE WRK-AREA-POOL1911
              STRING  CPFDI WRK-DIG-RET1
                      DELIMITED        BY SIZE
                                     INTO WRK-POOL1911-VALOR
              END-STRING
              MOVE    10               TO WRK-POOL1911-TAMANHO
              MOVE   'B'               TO WRK-POOL1911-DIGITO

              EXEC CICS
                   LINK PROGRAM  ('POOL1911')
                        COMMAREA (WRK-AREA-POOL1911)
                        LENGTH   (LENGTH OF WRK-AREA-POOL1911)
              END-EXEC

              IF WRK-POOL1911-RETORNO  EQUAL ZEROS
                 IF WRK-POOL1911-DIGITO
                                       EQUAL 'P'
                    MOVE    ZEROS      TO WRK-POOL1911-DIGITO
                 END-IF

                 MOVE    WRK-POOL1911-DIGITO
                                       TO WRK-DIG-RET2
                 MOVE    WRK-DIGITO-RETORNADO
                                       TO WRK-DIGITO-AUX
              END-IF

              IF  WRK-POOL1911-RETORNO NOT EQUAL ZEROS OR
                  WRK-DIGITO-AUX       NOT EQUAL CONTRDI OR
                  EIBRESP              NOT EQUAL DFHRESP(NORMAL)
                  MOVE   '*'           TO WRK-INCONSISTENTE
                  MOVE   -1            TO CONTRDL
                  MOVE   'R'           TO CONTRDA
                  MOVE    WRK-MSG08    TO MENSAGO
                  PERFORM 0410-ENVIAR-MAPA
              END-IF
           END-IF.

      *----------------------------------------------------------------*
       0814-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       0815-CONSISTIR-DATA             SECTION.
      *----------------------------------------------------------------*

           MOVE    WRK-DATA-AUX-INV-R  TO WRK-ENVIADA
           MOVE    SPACES              TO WRK-OPCAO

           EXEC CICS
                LINK PROGRAM  ('POOL3205')
                     COMMAREA (WRK-AREA-POOL3205)
                     LENGTH   (LENGTH OF WRK-AREA-POOL3205)
           END-EXEC

           IF EIBRESP                  NOT EQUAL DFHRESP(NORMAL)
              MOVE   'I'               TO WRK-OPCAO
           END-IF.

      *----------------------------------------------------------------*
       0815-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       0816-VALIDAR-CPF-OPERACAO       SECTION.
      *----------------------------------------------------------------*

           INITIALIZE WRK-AREA-POOL1911
           MOVE    CPFOI               TO WRK-POOL1911-VALOR (1:9)
           MOVE    09                  TO WRK-POOL1911-TAMANHO
           MOVE   'A'                  TO WRK-POOL1911-DIGITO

           EXEC CICS
                LINK PROGRAM  ('POOL1911')
                     COMMAREA (WRK-AREA-POOL1911)
                     LENGTH   (LENGTH OF WRK-AREA-POOL1911)
           END-EXEC

           IF WRK-POOL1911-RETORNO     NOT EQUAL ZEROS OR
              EIBRESP                  NOT EQUAL DFHRESP(NORMAL)
              MOVE   '*'               TO WRK-INCONSISTENTE
              MOVE   -1                TO CONTROPL
              MOVE   'R'               TO CONTROPA
              MOVE    WRK-MSG08        TO MENSAGO
              PERFORM 0410-ENVIAR-MAPA

           ELSE
              IF WRK-POOL1911-DIGITO   EQUAL 'P'
                 MOVE    ZEROS         TO WRK-POOL1911-DIGITO
              END-IF

              MOVE    WRK-POOL1911-DIGITO
                                       TO WRK-DIG-RET1
                                          WRK-RETORNADO

              INITIALIZE WRK-AREA-POOL1911
              STRING  CPFOI WRK-DIG-RET1
                      DELIMITED        BY SIZE
                                     INTO WRK-POOL1911-VALOR
              END-STRING
              MOVE    10               TO WRK-POOL1911-TAMANHO
              MOVE   'B'               TO WRK-POOL1911-DIGITO

              EXEC CICS
                   LINK PROGRAM  ('POOL1911')
                        COMMAREA (WRK-AREA-POOL1911)
                        LENGTH   (LENGTH OF WRK-AREA-POOL1911)
              END-EXEC

              IF WRK-POOL1911-RETORNO  EQUAL ZEROS
                 IF WRK-POOL1911-DIGITO
                                       EQUAL 'P'
                    MOVE    ZEROS      TO WRK-POOL1911-DIGITO
                 END-IF

                 MOVE    WRK-POOL1911-DIGITO
                                       TO WRK-DIG-RET2
                 MOVE    WRK-DIGITO-RETORNADO
                                       TO WRK-DIGITO-AUX
              END-IF

              IF WRK-POOL1911-RETORNO  NOT EQUAL ZEROS OR
                 WRK-DIGITO-AUX        NOT EQUAL CONTROPI OR
                 EIBRESP               NOT EQUAL DFHRESP(NORMAL)
                 MOVE   '*'            TO WRK-INCONSISTENTE
                 MOVE   -1             TO CONTROPL
                 MOVE   'R'            TO CONTROPA
                 MOVE    WRK-MSG08     TO MENSAGO
                 PERFORM 0410-ENVIAR-MAPA
              END-IF
           END-IF.

      *----------------------------------------------------------------*
       0816-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       0817-PROTEGER-CAMPOS            SECTION.
      *----------------------------------------------------------------*

           MOVE   '1'                  TO FILIALEA
                                          CONTREA
                                          FILIALFA
                                          CONTRFA
                                          FILIALCA
                                          CONTRCA
                                          CPFDA
                                          CONTRDA
                                          DDNASCDA
                                          MMNASCDA
                                          AANASCDA
                                          DDDDA
                                          TELDA
                                          CPFOA
                                          CONTROPA
                                          DDNASCOA
                                          MMNASCOA
                                          AANASCOA
                                          DDDOA
                                          TELOA
                                          CPF3A
                                          CONTR3A
                                          CODFUNCA
           MOVE   '/'                  TO NOMEGRA
                                          ENTREGAA
                                          FATURAA
                                          FUNCDA
                                          NOMEDA
                                          MESMOSA
                                          FUNCOA
                                          NOMEOA
4SYSFB                                    EMAILA
4SYSFB                                    EMAIL2A.

      *----------------------------------------------------------------*
       0817-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       0818-VALIDAR-CPF-VENDEDOR       SECTION.
      *----------------------------------------------------------------*

           INITIALIZE WRK-AREA-POOL1911
           MOVE    CPF3I               TO WRK-POOL1911-VALOR (1:9)
           MOVE    09                  TO WRK-POOL1911-TAMANHO
           MOVE   'A'                  TO WRK-POOL1911-DIGITO

           EXEC CICS
                LINK PROGRAM  ('POOL1911')
                     COMMAREA (WRK-AREA-POOL1911)
                     LENGTH   (LENGTH OF WRK-AREA-POOL1911)
           END-EXEC

           IF WRK-POOL1911-RETORNO     NOT EQUAL ZEROS OR
              EIBRESP                  NOT EQUAL DFHRESP(NORMAL)
              MOVE   '*'               TO WRK-INCONSISTENTE
              MOVE   -1                TO CONTR3L
              MOVE   'R'               TO CONTR3A
              MOVE    WRK-MSG08        TO MENSAGO
              PERFORM 0410-ENVIAR-MAPA

           ELSE
              IF WRK-POOL1911-DIGITO   EQUAL 'P'
                 MOVE    ZEROS         TO WRK-POOL1911-DIGITO
              END-IF

              MOVE    WRK-POOL1911-DIGITO
                                       TO WRK-DIG-RET1
                                          WRK-RETORNADO

              INITIALIZE WRK-AREA-POOL1911
              STRING  CPF3I WRK-DIG-RET1
                      DELIMITED        BY SIZE
                                     INTO WRK-POOL1911-VALOR
              END-STRING
              MOVE    10               TO WRK-POOL1911-TAMANHO
              MOVE   'B'               TO WRK-POOL1911-DIGITO

              EXEC CICS
                   LINK PROGRAM  ('POOL1911')
                        COMMAREA (WRK-AREA-POOL1911)
                        LENGTH   (LENGTH OF WRK-AREA-POOL1911)
              END-EXEC

              IF WRK-POOL1911-RETORNO  EQUAL ZEROS
                 IF WRK-POOL1911-DIGITO
                                       EQUAL 'P'
                    MOVE    ZEROS      TO WRK-POOL1911-DIGITO
                 END-IF

                 MOVE    WRK-POOL1911-DIGITO
                                       TO WRK-DIG-RET2
                 MOVE    WRK-DIGITO-RETORNADO
                                       TO WRK-DIGITO-AUX
              END-IF

              IF WRK-POOL1911-RETORNO  NOT EQUAL ZEROS OR
                 WRK-DIGITO-AUX        NOT EQUAL CONTR3I OR
                 EIBRESP               NOT EQUAL DFHRESP(NORMAL)
                 MOVE   '*'            TO WRK-INCONSISTENTE
                 MOVE   -1             TO CONTR3L
                 MOVE   'R'            TO CONTR3A
                 MOVE    WRK-MSG08     TO MENSAGO
                 PERFORM 0410-ENVIAR-MAPA
              END-IF
           END-IF.

      *----------------------------------------------------------------*
       0818-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

4SYSFB*----------------------------------------------------------------*
.      0819-CONSISTE-EMAIL             SECTION.
.     *----------------------------------------------------------------*
.
.     * CONSISTE PRIMEIRA POSICAO
.          IF (WRK-DIG-EMAIL(1)        NOT ALPHABETIC              AND
.              WRK-DIG-EMAIL(1)        NOT NUMERIC               ) OR
.             (WRK-DIG-EMAIL(1)        EQUAL SPACES OR LOW-VALUES)
.              MOVE 'S'                TO WRK-VALIDAR-EMAIL
.              MOVE WRK-MSG22          TO MENSAGO
               GO                      TO 0819-99-FIM
.          END-IF
.
.     * CONSISTE ULTIMA POSICAO DO EMAIL (40)
.          IF  WRK-DIG-EMAIL(40)       NOT EQUAL SPACES
.              IF  WRK-DIG-EMAIL(40)   NOT ALPHABETIC AND
.                  WRK-DIG-EMAIL(40)   NOT NUMERIC
.                  MOVE 'S'            TO WRK-VALIDAR-EMAIL
.                  MOVE WRK-MSG23      TO MENSAGO
                   GO                  TO 0819-99-FIM
.              END-IF
.          END-IF
.
.     * CONSISTE EMAIL TODO ATE ENCONTRAR ARROBA
.          PERFORM VARYING IND-4       FROM 2 BY 1
.                    UNTIL IND-4       GREATER 37 OR
.            WRK-DIG-EMAIL(IND-4)      EQUAL '@'
.
.              IF  WRK-DIG-EMAIL(IND-4)
.                                      NOT NUMERIC    AND
.                  WRK-DIG-EMAIL(IND-4)
.                                      NOT ALPHABETIC AND
.                  WRK-DIG-EMAIL(IND-4)
.                                      NOT EQUAL '_'  AND
.                  WRK-DIG-EMAIL(IND-4)
.                                      NOT EQUAL '-'  AND
.                  WRK-DIG-EMAIL(IND-4)
.                                      NOT EQUAL '.'
.                  MOVE 'S'            TO WRK-VALIDAR-EMAIL
.                  MOVE WRK-MSG30      TO MENSAGO
.                  GO                  TO 0819-99-FIM
.              END-IF
.
.          END-PERFORM.
.
.     *    NAO ENCONTROU ARROBA(@)
.
.          IF IND-4                    GREATER 37
.             MOVE 'S'                 TO WRK-VALIDAR-EMAIL
.             MOVE WRK-MSG24           TO MENSAGO
.             GO                       TO 0819-99-FIM
.          END-IF
.
.     *    CONSISTE CARACTER ANTES DO ARROBA(@)
.
.          ADD -1                      TO IND-4
.
.          IF  WRK-DIG-EMAIL(IND-4)    EQUAL '.'
.              MOVE 'S'                TO WRK-VALIDAR-EMAIL
.              MOVE WRK-MSG25          TO MENSAGO
.              GO                      TO 0819-99-FIM
.          ELSE
.              ADD 2                   TO IND-4
.          END-IF
.
.     *    CONSISTE CARACTER APOS ARROBA(@)
.
.          IF (WRK-DIG-EMAIL(IND-4)    NOT NUMERIC    AND
.              WRK-DIG-EMAIL(IND-4)    NOT ALPHABETIC) OR
.             (WRK-DIG-EMAIL(IND-4)    EQUAL SPACES OR LOW-VALUES)
.              MOVE 'S'                TO WRK-VALIDAR-EMAIL
.              MOVE WRK-MSG26          TO MENSAGO
               GO                      TO 0819-99-FIM
.          ELSE
.              ADD 1                   TO IND-4
.          END-IF
.
.     *    CONSISTE A PARTIR DO SEGUNDO CARACTER APOS @
.
.          MOVE  'N'                   TO WRK-PONTO
.
.          PERFORM VARYING IND-4       FROM IND-4 BY 1
.                    UNTIL IND-4       GREATER 39
.
.     *    CONSISTE ANTES DO PONTO DEVE SER NUMERO OU LETRA
.
.          IF  WRK-DIG-EMAIL(IND-4)    EQUAL '.'
.              MOVE 'S'                TO  WRK-PONTO
.              ADD -1                  TO  IND-4
.              IF  WRK-DIG-EMAIL(IND-4)
.                                      NOT NUMERIC    AND
.                  WRK-DIG-EMAIL(IND-4)
.                                      NOT ALPHABETIC
.                  MOVE 'S'            TO WRK-VALIDAR-EMAIL
.                  MOVE WRK-MSG27      TO MENSAGO
.                  GO                  TO 0819-99-FIM
.              ELSE
.                  ADD 1               TO IND-4
.              END-IF
.          END-IF
.
.     *    CONSISTE DEPOIS DO PONTO DEVE SER NUMERO OU LETRA
.
.          IF  WRK-DIG-EMAIL(IND-4)    EQUAL '.'
.              MOVE  'S'               TO WRK-PONTO
.              ADD   1                 TO IND-4
.              IF (WRK-DIG-EMAIL(IND-4)
.                                      EQUAL SPACES)   OR
.                 (WRK-DIG-EMAIL(IND-4)
.                                      NOT ALPHABETIC AND
.                  WRK-DIG-EMAIL(IND-4)
.                                      NOT NUMERIC)
.                  MOVE 'S'            TO WRK-VALIDAR-EMAIL
.                  MOVE WRK-MSG28      TO MENSAGO
.                  GO                  TO 0819-99-FIM
.              END-IF
.          END-IF
.
.          IF  WRK-DIG-EMAIL(IND-4)    NOT EQUAL '.' AND
.              WRK-DIG-EMAIL(IND-4)    NOT ALPHABETIC AND
.              WRK-DIG-EMAIL(IND-4)    NOT NUMERIC    AND
.              WRK-DIG-EMAIL(IND-4)    NOT EQUAL '-'
.              MOVE 'S'                TO WRK-VALIDAR-EMAIL
.              MOVE WRK-MSG29          TO MENSAGO
.              GO                      TO 0819-99-FIM
.          END-IF
.
.          END-PERFORM.
.
.          IF WRK-PONTO                EQUAL 'N'
.             MOVE 'S'                 TO WRK-VALIDAR-EMAIL
.             MOVE WRK-MSG31           TO MENSAGO
.          END-IF.
.
.     *----------------------------------------------------------------*
.      0819-99-FIM.                    EXIT.
4SYSFB*----------------------------------------------------------------*

      *----------------------------------------------------------------*
       0820-TRATAR-FASE-2              SECTION.
      *----------------------------------------------------------------*

           MOVE   '1'                  TO WRK-COMM-FASE
           MOVE   -1                   TO NOMEGRL
           MOVE    WRK-MSG03           TO MENSAGO
           PERFORM 0410-ENVIAR-MAPA.

      *----------------------------------------------------------------*
       0820-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       0900-TRATAR-PF-INVALIDA         SECTION.
      *----------------------------------------------------------------*

           EVALUATE WRK-COMM-FASE
               WHEN '1'
                    MOVE   -1          TO NOMEGRL

               WHEN '2'
                    PERFORM 0817-PROTEGER-CAMPOS
           END-EVALUATE

           MOVE    WRK-MSG01           TO MENSAGO
           PERFORM 0410-ENVIAR-MAPA.

      *----------------------------------------------------------------*
       0900-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       9998-TRATAR-ERRO                SECTION.
      *----------------------------------------------------------------*

           IF EIBRESP                  NOT EQUAL ZEROS
              MOVE    EIBRESP          TO WRK-EIBRESP
              MOVE    WRK-MSG99        TO MENSAGO

           ELSE
              PERFORM 9999-ERRO-ABCODE
              MOVE    WRK-MSG00        TO MENSAGO
           END-IF.

           MOVE   -1                   TO NOMEGRL
           MOVE   '1'                  TO WRK-COMM-FASE

           PERFORM 0410-ENVIAR-MAPA.

      *----------------------------------------------------------------*
       9998-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       9999-ERRO-ABCODE                SECTION.
      *----------------------------------------------------------------*

           EXEC CICS
                ASSIGN ABCODE(WRK-ABCODE-MSG)
           END-EXEC.

      *----------------------------------------------------------------*
       9999-99-FIM.                    EXIT.
      *----------------------------------------------------------------*
