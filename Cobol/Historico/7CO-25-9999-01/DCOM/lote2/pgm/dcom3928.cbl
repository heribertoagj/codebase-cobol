      *================================================================*
       IDENTIFICATION                  DIVISION.
      *================================================================*

       PROGRAM-ID. DCOM3928.
       AUTHOR.     LUCIANDRA SILVEIRA.
      *================================================================*
      *                          BSI TECNOLOGIA                        *
      *----------------------------------------------------------------*
      *     PROGRAMA....: DCOM3928                                     *
      *     PROGRAMADOR.: LUCIANDRA SILVEIRA - BSI TECNOLOGIA          *
      *     ANALISTA BSI: LUCIANDRA SILVEIRA - BSI TECNOLOGIA          *
      *     ANALISTA DDS: RICARDO JAMMAL     - BRADESCO - GRUPO 39     *
      *     DATA........: 12/06/2017                                   *
      *----------------------------------------------------------------*
      *     OBJETIVO....: CONSULTAR JUROS MORATORIOS.                  *
      *----------------------------------------------------------------*
      *     TELAS.......:                                              *
      *     DCOMIUTM....: CONSULTAR JUROS MORATORIOS.                  *
      *----------------------------------------------------------------*
      *     BOOKS.......:                                              *
      *     I#BRAD7C - AREA DE ERRO DA BRAD7100                        *
      *     I#DCOMJ4 - AREA DE ACESSO AO DCOM5537.                     *
      *----------------------------------------------------------------*
      *     MODULOS.....:                                              *
      *     BRAD0660 - INSERIR CARACTERES DE NULL NA MENSAGEM DE SAIDA.*
      *     BRAD7100 - TRATAMENTO DE ERROS QUANDO PROGRAMA INVALIDO.   *
      *     DCOM5169 - OBTER DADOS DE ENCARGOS MORATORIOS.             *
      *     DCOM5537 - CONSULTA TODOS OS DADOS DE UMA OPERACAO EM BASE *
      *                OFICIAL OU HISTORICA.                           *
      *----------------------------------------------------------------*
      *     NAVEGACAO...:                                              *
      *     CHAMADO POR.: DCOM1503  DCOM1504        CHAMA:    -        *
      *                   DCOM1505  DCOM1506                           *
      *                   DCOM1508  DCOM1510                           *
      *                   DCOM1518  DCOM1512                           *
      *                   DCOM1520  DCOM1519                           *
      *                   DCOM0630  DCOM0632                           *
      *                   DCOM0645  DCOM1527                           *
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
       77  FILLER                      PIC  X(050)         VALUE
           '*** DCOM3928 - INICIO DA AREA DE WORKING ***'.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       77  FILLER                      PIC  X(050)         VALUE
           '*** VARIAVEIS AUXILIARES ***'.
      *----------------------------------------------------------------*

       77  WRK-MODULO                  PIC  X(008)         VALUE SPACES.
       77  WRK-SQLCA                   PIC  X(136)         VALUE SPACES.
       77  WRK-INCONSIS                PIC  X(001)         VALUE SPACES.
       77  WRK-LOCAL-OPER              PIC  X(001)         VALUE SPACES.

       01  WRK-BCOAGECTA.
           05 WRK-BCO                  PIC  9(003)         VALUE ZEROS.
           05 FILLER                   PIC  X(001)         VALUE '/'.
           05 WRK-AGE                  PIC  9(005)         VALUE ZEROS.
           05 FILLER                   PIC  X(001)         VALUE '/'.
           05 WRK-CTA                  PIC  9(013)         VALUE ZEROS.

       01  WRK-CNPJCPF.
4S2511*    05 WRK-NRO-CNPJCPF          PIC  999.999.999    VALUE ZEROS.
4S2511     05 WRK-NRO-CNPJCPF          PIC  X(11)          VALUE SPACES.
           05 FILLER                   PIC  X(001)         VALUE '/'.
4S2511*    05 WRK-FIL-CNPJ             PIC  9(004)         VALUE ZEROS.
4S2511     05 WRK-FIL-CNPJ             PIC  X(04)          VALUE SPACES.
           05 FILLER                   PIC  X(001)         VALUE '-'.
           05 WRK-CTR-CNPJCPF          PIC  9(002)         VALUE ZEROS.

       01  WRK-CABEC-PARCELA.
           05 FILLER                   PIC  X(010)         VALUE
              'PARCELA.: '.
           05 WRK-NRO-PARCELA          PIC  9(005)         VALUE ZEROS.
           05 FILLER                   PIC  X(028)         VALUE
              '               SITUACAO...: '.
           05 WRK-SIT-PARCELA          PIC  X(016)         VALUE SPACES.
           05 FILLER                   PIC  X(010)         VALUE SPACES.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           '*** AREA PARA MENSAGENS ***'.
      *----------------------------------------------------------------*

       01  WRK-MSG001                  PIC  X(079)         VALUE
           'CONSULTA EFETUADA COM SUCESSO. TECLE <PF3> PARA RETORNAR'.
       01  WRK-MSG002                  PIC  X(079)         VALUE
           'PF INVALIDA'.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           '*** AREAS AUXILIARES PARA POOL5005 ***'.
      *----------------------------------------------------------------*

       01  WRK-POOL5005.
           05 WRK-GU                   PIC  X(004)         VALUE 'GU  '.
           05 WRK-CHNG                 PIC  X(004)         VALUE 'CHNG'.
           05 WRK-ISRT                 PIC  X(004)         VALUE 'ISRT'.
           05 WRK-FUNCAO               PIC  X(004)         VALUE SPACES.
           05 WRK-TELA                 PIC  X(008)         VALUE SPACES.
           05 WRK-VERSAO               PIC  X(006)         VALUE
              'VRS002'.

           05 WRK-COD-USER             PIC  X(007)         VALUE SPACES.
           05 WRK-COD-USER-R           REDEFINES
              WRK-COD-USER             PIC  9(007).

           05 WRK-COD-DEPTO.
              10 FILLER                PIC  X(002)         VALUE SPACES.
              10 WRK-COD-DEPTO-N       PIC  9(004)         VALUE ZEROS.

           05 WRK-MENSAGEM.
              10 FILLER                PIC  X(004)         VALUE SPACES.
              10 WRK-TRANSACAO         PIC  X(008)         VALUE SPACES.
              10 FILLER                PIC  X(1988)        VALUE SPACES.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           '*** AREA DA TELA DCOMIUTM - INPUT ***'.
      *----------------------------------------------------------------*

       01  WRK-INPUT-IUTM.
           05 FILLER                   PIC  X(018)         VALUE SPACES.
           05 WRK-INP-PFK-IUTM         PIC  X(001)         VALUE SPACES.
           05 WRK-INP-SENHAS-IUTM      PIC  X(037)         VALUE SPACES.
           05 WRK-INP-COMANDO-IUTM     PIC  X(068)         VALUE SPACES.
           05 WRK-INP-DADOS-IUTM.
              10 WRK-INP-TITULO-IUTM   PIC  X(048)         VALUE SPACES.
              10 WRK-INP-NOME-IUTM     PIC  X(040)         VALUE SPACES.
              10 WRK-INP-CNPJCPF-IUTM  PIC  X(019)         VALUE SPACES.
              10 WRK-INP-BCAGCTA-IUTM  PIC  X(023)         VALUE SPACES.
              10 WRK-INP-CPRODT-IUTM   PIC  9(003)         VALUE ZEROS.
              10 WRK-INP-IPRODT-IUTM   PIC  X(012)         VALUE SPACES.
              10 WRK-INP-CSPROD-IUTM   PIC  9(003)         VALUE ZEROS.
              10 WRK-INP-ISPROD-IUTM   PIC  X(012)         VALUE SPACES.
              10 WRK-INP-OPERAC-IUTM   PIC  9(013)         VALUE ZEROS.
              10 FILLER REDEFINES      WRK-INP-OPERAC-IUTM.
                 15 WRK-INP-DANO-IUTM  PIC  9(004).
                 15 WRK-INP-NSEQ-IUTM  PIC  9(009).
              10 WRK-INP-SITUAC-IUTM   PIC  X(015)         VALUE SPACES.
              10 WRK-INP-DESIDE1-IUTM  PIC  X(013)         VALUE SPACES.
              10 WRK-INP-NUMERO1-IUTM.
                 15 WRK-INP-NUMERO1-IUTM-N
                                       PIC  9(009)         VALUE ZEROS.
              10 WRK-INP-DESIDE2-IUTM  PIC  X(013)         VALUE SPACES.
              10 WRK-INP-NUMERO2-IUTM.
                 15 WRK-INP-NUMERO2-IUTM-N
                                       PIC  9(009)         VALUE ZEROS.
              10 WRK-INP-DETPARC-IUTM  PIC  X(069)         VALUE SPACES.
              10 WRK-INP-OCULTOS-IUTM.
                 15 WRK-INP-CHAMADOR-IUTM
                                       PIC  X(008)         VALUE SPACES.
                 15 WRK-INP-OPCAO-IUTM.
                    20 WRK-INP-OPCAO-IUTM-N
                                       PIC  9(001)         VALUE ZEROS.
                 15 WRK-INP-TIMESTAMP-IUTM
                                       PIC  X(026)         VALUE SPACES.
                 15 WRK-INP-LISTA-IUTM PIC  X(036)         VALUE SPACES.
                 15 WRK-INP-PGMANT-IUTM
                                       PIC  X(008)         VALUE SPACES.
                 15 WRK-INP-TMANUT-IUTM
                                       PIC  X(001)         VALUE SPACES.
                 15 WRK-INP-HULT-IUTM  PIC  X(026)         VALUE SPACES.
                 15 WRK-INP-HULTPARC-IUTM
                                       PIC  X(026)         VALUE SPACES.
                 15 WRK-INP-DEFETV-IUTM
                                       PIC  X(010)         VALUE SPACES.
                 15 WRK-INP-CNPJOC-IUTM
                                       PIC  9(009)         VALUE ZEROS.
                 15 WRK-INP-AREA-IUTM.
                    20 WRK-INP-AREA1-IUTM
                                       PIC  X(047)         VALUE SPACES.
                    20 WRK-INP-AREA2-IUTM
                                       PIC  X(005)         VALUE SPACES.
                    20 FILLER          PIC  X(019)         VALUE SPACES.
                    20 WRK-INP-CHAMADOR3-IUTM
                                       PIC  X(008)         VALUE SPACES.
                    20 FILLER          PIC  X(136)         VALUE SPACES.
                    20 WRK-INP-VPRINC-IUTM.
                       25 WRK-INP-VPRINC-IUTM-N
                                       PIC  9(009)V9(002)  VALUE ZEROS.
                    20 FILLER          PIC  X(018)         VALUE SPACES.
              10 WRK-INP-VJREMUN-IUTM  PIC  X(014)         VALUE SPACES.
              10 WRK-INP-PJREMUN-IUTM  PIC  X(009)         VALUE SPACES.
              10 WRK-INP-VJMORAT-IUTM  PIC  X(014)         VALUE SPACES.
              10 WRK-INP-PJMORAT-IUTM  PIC  X(009)         VALUE SPACES.
              10 WRK-INP-VJMULTA-IUTM  PIC  X(014)         VALUE SPACES.
              10 WRK-INP-PJMULTA-IUTM  PIC  X(009)         VALUE SPACES.
           05 WRK-INP-MENSA-IUTM       PIC  X(079)         VALUE SPACES.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           '*** AREA DA TELA DCOMIUTM  - OUTPUT ***'.
      *----------------------------------------------------------------*

       01  WRK-OUTPUT-IUTM.
           05 WRK-OUT-LL-IUTM          PIC S9(004) COMP    VALUE ZEROS.
           05 WRK-OUT-ZZ-IUTM          PIC S9(004) COMP    VALUE ZEROS.
           05 WRK-OUT-SENHAS-IUTM      PIC  X(037)         VALUE SPACES.
           05 WRK-OUT-COMANDO-IUTM     PIC  X(068)         VALUE SPACES.
           05 WRK-OUT-DADOS-IUTM.
              10 WRK-OUT-TITULO-IUTM   PIC  X(048)         VALUE SPACES.
              10 WRK-OUT-NOME-IUTM     PIC  X(040)         VALUE SPACES.
              10 WRK-OUT-CNPJCPF-IUTM  PIC  X(019)         VALUE SPACES.
              10 WRK-OUT-BCAGCTA-IUTM  PIC  X(023)         VALUE SPACES.
              10 WRK-OUT-CPRODT-IUTM   PIC  9(003)         VALUE ZEROS.
              10 WRK-OUT-IPRODT-IUTM   PIC  X(012)         VALUE SPACES.
              10 WRK-OUT-CSPROD-IUTM   PIC  9(003)         VALUE ZEROS.
              10 WRK-OUT-ISPROD-IUTM   PIC  X(012)         VALUE SPACES.
              10 WRK-OUT-OPERAC-IUTM   PIC  9(013)         VALUE ZEROS.
              10 FILLER REDEFINES      WRK-OUT-OPERAC-IUTM.
                 15 WRK-OUT-DANO-IUTM  PIC  9(004).
                 15 WRK-OUT-NSEQ-IUTM  PIC  9(009).
              10 WRK-OUT-SITUAC-IUTM   PIC  X(015)         VALUE SPACES.
              10 WRK-OUT-DESIDE1-IUTM  PIC  X(013)         VALUE SPACES.
              10 WRK-OUT-NUMERO1-IUTM.
                 15 WRK-OUT-NUMERO1-IUTM-N
                                       PIC  9(009)         VALUE ZEROS.
              10 WRK-OUT-DESIDE2-IUTM  PIC  X(013)         VALUE SPACES.
              10 WRK-OUT-NUMERO2-IUTM.
                 15 WRK-OUT-NUMERO2-IUTM-N
                                       PIC  9(009)         VALUE ZEROS.
              10 WRK-OUT-DETPARC-IUTM  PIC  X(069)         VALUE SPACES.
              10 WRK-OUT-OCULTOS-IUTM.
                 15 WRK-OUT-CHAMADOR-IUTM
                                       PIC  X(008)         VALUE SPACES.
                 15 WRK-OUT-OPCAO-IUTM.
                    20 WRK-OUT-OPCAO-IUTM-N
                                       PIC  9(001)         VALUE ZEROS.
                 15 WRK-OUT-TIMESTAMP-IUTM
                                       PIC  X(026)         VALUE SPACES.
                 15 WRK-OUT-LISTA-IUTM PIC  X(036)         VALUE SPACES.
                 15 WRK-OUT-PGMANT-IUTM
                                       PIC  X(008)         VALUE SPACES.
                 15 WRK-OUT-TMANUT-IUTM
                                       PIC  X(001)         VALUE SPACES.
                 15 WRK-OUT-HULT-IUTM  PIC  X(026)         VALUE SPACES.
                 15 WRK-OUT-HULTPARC-IUTM
                                       PIC  X(026)         VALUE SPACES.
                 15 WRK-OUT-DEFETV-IUTM
                                       PIC  X(010)         VALUE SPACES.
                 15 WRK-OUT-CNPJOC-IUTM
                                       PIC  9(009)         VALUE ZEROS.
                 15 WRK-OUT-AREA-IUTM.
                    20 WRK-OUT-AREA1-IUTM
                                       PIC  X(047)         VALUE SPACES.
                    20 WRK-OUT-AREA2-IUTM
                                       PIC  X(005)         VALUE SPACES.
                    20 FILLER          PIC  X(163)         VALUE SPACES.
                    20 WRK-OUT-VPRINC-IUTM.
                       25 WRK-OUT-VPRINC-IUTM-N
                                       PIC  9(009)V9(002)  VALUE ZEROS.
                    20 FILLER          PIC  X(018)         VALUE SPACES.
              10 WRK-OUT-VJREMUN-IUTM.
                 15 WRK-OUT-VJREMUN-IUTM-N
                                       PIC  ZZZ.ZZZ.ZZ9,99 VALUE ZEROS.
              10 WRK-OUT-PJREMUN-IUTM.
                 15 WRK-OUT-PJREMUN-IUTM-N
                                       PIC  ZZ9,99999      VALUE ZEROS.
              10 WRK-OUT-VJMORAT-IUTM.
                 15 WRK-OUT-VJMORAT-IUTM-N
                                       PIC  ZZZ.ZZZ.ZZ9,99 VALUE ZEROS.
              10 WRK-OUT-PJMORAT-IUTM.
                 15 WRK-OUT-PJMORAT-IUTM-N
                                       PIC  ZZ9,99999      VALUE ZEROS.
              10 WRK-OUT-VJMULTA-IUTM.
                 15 WRK-OUT-VJMULTA-IUTM-N
                                       PIC  ZZZ.ZZZ.ZZ9,99 VALUE ZEROS.
              10 WRK-OUT-PJMULTA-IUTM.
                 15 WRK-OUT-PJMULTA-IUTM-N
                                       PIC  ZZ9,99999      VALUE ZEROS.
           05 WRK-OUT-MENSA-IUTM       PIC  X(079)         VALUE SPACES.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           '*** AREA DA TELA DCOMIUTM - BRAD0660 ***'.
      *----------------------------------------------------------------*

       01  WRK-660-DCOMIUTM.
           05 WRK-660-LL-AREA-IUTM     PIC  9(004) COMP    VALUE 0000.
           05 WRK-660-LL-MENSAG-IUTM   PIC  9(004) COMP    VALUE 0000.
           05 WRK-660-SENHAS-IUTM      PIC  9(004) COMP    VALUE 0037.
           05 WRK-660-COMANDO-IUTM     PIC  9(004) COMP    VALUE 0068.
           05 WRK-660-TITULO-IUTM      PIC  9(004) COMP    VALUE 0048.
           05 WRK-660-NOME-IUTM        PIC  9(004) COMP    VALUE 0040.
           05 WRK-660-CNPJCPF-IUTM     PIC  9(004) COMP    VALUE 0019.
           05 WRK-660-BCAGCTA-IUTM     PIC  9(004) COMP    VALUE 0023.
           05 WRK-660-CPRODT-IUTM      PIC  9(004) COMP    VALUE 0003.
           05 WRK-660-IPRODT-IUTM      PIC  9(004) COMP    VALUE 0012.
           05 WRK-660-CSPROD-IUTM      PIC  9(004) COMP    VALUE 0003.
           05 WRK-660-ISPROD-IUTM      PIC  9(004) COMP    VALUE 0012.
           05 WRK-660-OPERAC-IUTM      PIC  9(004) COMP    VALUE 0013.
           05 WRK-660-SITUAC-IUTM      PIC  9(004) COMP    VALUE 0015.
           05 WRK-660-DESIDE1-IUTM     PIC  9(004) COMP    VALUE 0013.
           05 WRK-660-NUMERO1-IUTM     PIC  9(004) COMP    VALUE 0009.
           05 WRK-660-DESIDE2-IUTM     PIC  9(004) COMP    VALUE 0013.
           05 WRK-660-NUMERO2-IUTM     PIC  9(004) COMP    VALUE 0009.
           05 WRK-660-DETPAR-IUTM      PIC  9(004) COMP    VALUE 0069.
           05 WRK-660-OCULTOS1-IUTM    PIC  9(004) COMP    VALUE 0079.
           05 WRK-660-OCULTOS2-IUTM    PIC  9(004) COMP    VALUE 0079.
           05 WRK-660-OCULTOS3-IUTM    PIC  9(004) COMP    VALUE 0079.
           05 WRK-660-OCULTOS4-IUTM    PIC  9(004) COMP    VALUE 0079.
           05 WRK-660-OCULTOS5-IUTM    PIC  9(004) COMP    VALUE 0079.
           05 WRK-660-VJREMUN-IUTM     PIC  9(004) COMP    VALUE 0014.
           05 WRK-660-PJREMUN-IUTM     PIC  9(004) COMP    VALUE 0009.
           05 WRK-660-VJMORAT-IUTM     PIC  9(004) COMP    VALUE 0014.
           05 WRK-660-PJMORAT-IUTM     PIC  9(004) COMP    VALUE 0009.
           05 WRK-660-VJMULTA-IUTM     PIC  9(004) COMP    VALUE 0014.
           05 WRK-660-PJMULTA-IUTM     PIC  9(004) COMP    VALUE 0009.
           05 WRK-660-MENSA-IUTM       PIC  9(004) COMP    VALUE 0079.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           '*** AREA DE ERRO DA BRAD0660 ***'.
      *----------------------------------------------------------------*

       01  WRK-ERRO-BRAD0660.
           05 FILLER                   PIC  X(045)         VALUE
              '* RETURN-CODE DIFERENTE DE ZEROS NA BRAD0660'.
           05 FILLER                   PIC  X(012)         VALUE
              '- RET.COD ='.
           05 WRK-RETURN-CODE          PIC  9(002)         VALUE ZEROS.
           05 FILLER                   PIC  X(011)         VALUE
              ' - LOCAL ='.
           05 WRK-LOCAL-ERRO           PIC  9(002)         VALUE ZEROS.
           05 FILLER                   PIC  X(002)         VALUE ' *'.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           '*** AREA DE COMUNICACAO ***'.
      *----------------------------------------------------------------*

       01  WRK-COMU-AREA.
           05 WRK-COMU-LL              PIC S9(004) COMP    VALUE ZEROS.
           05 WRK-COMU-ZZ              PIC S9(004) COMP    VALUE ZEROS.
           05 WRK-COMU-TRANCODE.
              10 WRK-COMU-TRANSACAO    PIC  X(008)         VALUE SPACES.
              10 FILLER                PIC  X(006)         VALUE SPACES.
              10 WRK-COMU-PFK          PIC  X(001)         VALUE SPACES.
           05 WRK-COMU-SENHAS          PIC  X(037)         VALUE SPACES.
           05 WRK-COMU-COMANDO         PIC  X(068)         VALUE SPACES.
           05 WRK-COMU-DADOS-06.
              10 WRK-COMU-TMSTAMP-06   PIC  X(026)         VALUE SPACES.
              10 WRK-COMU-MENSA-06     PIC  X(079)         VALUE SPACES.
              10 WRK-COMU-DANO-06      PIC  9(004)         VALUE ZEROS.
              10 WRK-COMU-NSEQ-06      PIC  9(009)         VALUE ZEROS.
              10 WRK-COMU-NPCELA-06    PIC  9(005)         VALUE ZEROS.
              10 WRK-COMU-HULT-PCELA-06
                                       PIC  X(026)         VALUE SPACES.
              10 WRK-COMU-OPCAO-06     PIC  9(001)         VALUE ZEROS.
              10 WRK-COMU-HULT-BAIXA-06
                                       PIC  X(026)         VALUE SPACES.

              10 WRK-COMU-DEFETV-BAIXA-06
                                       PIC  X(010)         VALUE SPACES.
              10 FILLER                PIC  X(094)         VALUE SPACES.
           05 WRK-COMU-DADOS-15A       REDEFINES WRK-COMU-DADOS-06.
              10 WRK-COMU-OPCAO-15A    PIC  X(001).
              10 WRK-COMU-TMSTAMP-15A  PIC  X(026).
              10 WRK-COMU-DANO-15A     PIC  9(004).
              10 WRK-COMU-NSEQ-15A     PIC  9(009).
              10 WRK-COMU-MENSA-15A    PIC  X(079).
              10 FILLER                PIC  X(161).
           05 WRK-COMU-DADOS-15B       REDEFINES WRK-COMU-DADOS-06.
              10 WRK-COMU-OPCAO-15B    PIC  X(001).
              10 WRK-COMU-TMSTAMP-15B  PIC  X(026).
              10 WRK-COMU-LISTA-15B    PIC  X(036).
              10 WRK-COMU-DANO-15B     PIC  9(004).
              10 WRK-COMU-NSEQ-15B     PIC  9(009).
              10 WRK-COMU-TRANS-15B    PIC  X(008).
              10 WRK-COMU-MENSA-15B    PIC  X(079).
              10 FILLER                PIC  X(117).
           05 WRK-COMU-DADOS-15C       REDEFINES WRK-COMU-DADOS-06.
              10 WRK-COMU-OPCAO-15C    PIC  X(001).
              10 WRK-COMU-TMSTAMP-15C  PIC  X(026).
              10 WRK-COMU-DANO-15C     PIC  9(004).
              10 WRK-COMU-NSEQ-15C     PIC  9(009).
              10 WRK-COMU-PCELA-15C    PIC  9(005).
              10 WRK-COMU-MENSA-15C    PIC  X(079).
              10 FILLER                PIC  X(156).
           05 WRK-COMU-DADOS-15D       REDEFINES WRK-COMU-DADOS-06.
              10 WRK-COMU-OPCAO-15D    PIC  X(001).
              10 WRK-COMU-TMSTAMP-15D  PIC  X(026).
              10 WRK-COMU-DANO-15D     PIC  9(004).
              10 WRK-COMU-NSEQ-15D     PIC  9(009).
              10 WRK-COMU-PCELA-15D    PIC  9(005).
              10 WRK-COMU-MENSA-15D    PIC  X(079).
              10 FILLER                PIC  X(156).
           05 WRK-COMU-DADOS-15E       REDEFINES WRK-COMU-DADOS-06.
              10 WRK-COMU-OPCAO-15E    PIC  X(001).
              10 WRK-COMU-TPMANUT-15E  PIC  X(001).
              10 WRK-COMU-TMSTAMP-15E  PIC  X(026).
              10 WRK-COMU-LISTA-15E    PIC  X(036).
              10 WRK-COMU-DANO-15E     PIC  9(004).
              10 WRK-COMU-NSEQ-15E     PIC  9(009).
              10 WRK-COMU-MENSA-15E    PIC  X(079).
              10 WRK-COMU-PCELA-15E    PIC  9(005).
              10 WRK-COMU-PGMANT-15E   PIC  X(008).
              10 FILLER                PIC  X(111).
           05 WRK-COMU-DADOS-17        REDEFINES WRK-COMU-DADOS-06.
              10 WRK-COMU-OPCAO-17     PIC  9(001).
              10 WRK-COMU-TMSTAMP-17   PIC  X(026).
              10 WRK-COMU-DANO-17      PIC  9(004).
              10 WRK-COMU-NSEQ-17      PIC  9(009).
              10 WRK-COMU-NPCELA-17    PIC  9(005).
              10 WRK-COMU-OUTROS1-17   PIC  X(047).
              10 WRK-COMU-MENSA-17     PIC  X(079).
              10 WRK-COMU-OUTROS2-17   PIC  X(005).
              10 WRK-COMU-CHAMADOR-17  PIC  X(008).
              10 FILLER                PIC  X(096).
           05 WRK-COMU-DADOS-24        REDEFINES WRK-COMU-DADOS-06.
              10 WRK-COMU-OPCAO-24     PIC  9(001).
              10 WRK-COMU-TMSTAMP-24   PIC  X(026).
              10 WRK-COMU-DANO-24      PIC  9(004).
              10 WRK-COMU-NSEQ-24      PIC  9(009).
              10 WRK-COMU-AREA-24.
                 15 FILLER             PIC  9(005).
                 15 WRK-COMU-NPCELA-24 PIC  9(005).
                 15 FILLER             PIC  X(061).
                 15 WRK-COMU-CHAMADOR3-24
                                       PIC  X(008).
                 15 FILLER             PIC  X(012).
                 15 WRK-COMU-MENSA-24  PIC  X(079).
                 15 WRK-COMU-DVCTO-PARC-24
                                       PIC  X(010).
                 15 WRK-COMU-VLR-PARCELA-24
                                       PIC  9(009)V9(002).
                 15 WRK-COMU-CSIT-DESC-24
                                       PIC  9(003).
                 15 WRK-COMU-IRSUMO-SIT-24
                                       PIC  X(015).
                 15 WRK-COMU-LOCAL-OPER-24
                                       PIC  X(001).
                 15 WRK-COMU-QTD-CHEQUES-24
                                       PIC  9(005).
                 15 WRK-COMU-VPRINC-PCELA-24
                                       PIC  9(009)V9(002).
                 15 WRK-COMU-VEFETIV-BAIXA-24
                                       PIC  9(009)V9(002).
                 15 FILLER             PIC  X(003).

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           '*** AREA DE ACESSO AO MODULO DCOM5169 ***'.
      *----------------------------------------------------------------*

       01  WRK-5169-ENTRADA.
           05 WRK-5169E-COD-RETORNO    PIC  X(004)         VALUE SPACES.
           05 WRK-5169E-MSG-RETORNO    PIC  X(079)         VALUE SPACES.
           05 WRK-5169E-RESTART        PIC  9(005)         VALUE ZEROS.
           05 WRK-5169E-FLAG           PIC  X(001)         VALUE SPACES.
           05 WRK-5169E-CFUNC-BDSCO    PIC  9(009)         VALUE ZEROS.
           05 WRK-5169E-CTERM          PIC  X(008)         VALUE SPACES.
           05 WRK-5169E-AGENCIA-FUNC   PIC  9(005)         VALUE ZEROS.
           05 WRK-5169E-DANO-OPER-DESC PIC  9(004)         VALUE ZEROS.
           05 WRK-5169E-NSEQ-OPER-DESC PIC  9(009)         VALUE ZEROS.
           05 WRK-5169E-NPCELA-DESC-COML
                                       PIC  9(005)         VALUE ZEROS.
           05 WRK-5169E-HULT-ATULZ     PIC  X(026)         VALUE SPACES.
           05 WRK-5169E-HBAIXA-PCELA   PIC  X(026)         VALUE SPACES.
           05 WRK-5169E-DEFETV-BAIXA   PIC  X(010)         VALUE SPACES.
           05 WRK-5169E-LOCAL          PIC  X(001)         VALUE SPACES.
           05 WRK-5169E-OPCAO-SENHAS   PIC  9(002)         VALUE ZEROS.
           05 WRK-5169E-OPCAO-MENU     PIC  9(001)         VALUE ZEROS.
           05 WRK-5169E-FUNCAO         PIC  X(002)         VALUE SPACES.
4S2511     05 WRK-5169E-CCNPJ-CPF      PIC  X(009)         VALUE SPACES.
4S2511     05 WRK-5169E-CFLIAL-CNPJ    PIC  X(04)          VALUE SPACES.
           05 WRK-5169E-CCTRL-CNPJ-CPF PIC  9(002)         VALUE ZEROS.
           05 WRK-5169E-VLR-RECALCULO  PIC  9(015)V9(002)  VALUE ZEROS.
           05 FILLER                   PIC  X(071)         VALUE SPACES.

       01  WRK-5169-SAIDA.
           05 WRK-5169S-COD-RETORNO    PIC  9(004)         VALUE ZEROS.
           05 WRK-5169S-MSG-RETORNO    PIC  X(079)         VALUE SPACES.
           05 WRK-5169S-CSIT-DESC-COML PIC  9(003)         VALUE ZEROS.
           05 WRK-5169S-ISIT-DESC-COML PIC  X(030)         VALUE SPACES.
           05 WRK-5169S-VIOF-PCELA-VENCD
                                       PIC  9(015)V9(002)  VALUE ZEROS.
           05 WRK-5169S-VJURO-REMUN-VENCD
                                       PIC  9(015)V9(002)  VALUE ZEROS.
           05 WRK-5169S-PJURO-REMUN-VENCD
                                       PIC  9(003)V9(005)  VALUE ZEROS.
           05 WRK-5169S-VMORA-VENCD-DESC
                                       PIC  9(015)V9(002)  VALUE ZEROS.
           05 WRK-5169S-PMORA-VENCD-DESC
                                       PIC  9(003)V9(005)  VALUE ZEROS.
           05 WRK-5169S-VMULTA-VENCD-DESC
                                       PIC  9(015)V9(002)  VALUE ZEROS.
           05 WRK-5169S-PMULTA-VENCD-DESC
                                       PIC  9(003)V9(005)  VALUE ZEROS.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           '*** AREA DE ACESSO AO MODULO DCOM5537 ***'.
      *----------------------------------------------------------------*

       COPY 'I#DCOMJ4'.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           '*** AREA DA BRAD7100 ***'.
      *----------------------------------------------------------------*

       COPY 'I#BRAD7C'.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           '*** DCOM3928 - FIM DA AREA DE WORKING ***'.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       LINKAGE                         SECTION.
      *----------------------------------------------------------------*

       01  LNK-IO-PCB.
           05 LNK-IO-LTERM             PIC  X(008).
           05 FILLER                   PIC  X(002).
           05 LNK-IO-STATUS            PIC  X(002).
           05 FILLER                   PIC  X(012).
           05 LNK-IO-MODNAME           PIC  X(008).

       01  LNK-ALT-PCB.
           05 LNK-ALT-LTERM            PIC  X(008).
           05 FILLER                   PIC  X(002).
           05 LNK-ALT-STATUS           PIC  X(002).
           05 FILLER                   PIC  X(012).
           05 LNK-ALT-MODNAME          PIC  X(008).

      *================================================================*
       PROCEDURE                       DIVISION.
      *================================================================*

           ENTRY 'DLITCBL'             USING LNK-IO-PCB
                                             LNK-ALT-PCB.

           MOVE WRK-GU                 TO WRK-FUNCAO.

      *----------------------------------------------------------------*
      *     PROCEDIMENTOS INICIAIS                                     *
      *----------------------------------------------------------------*
       0000-INICIAR                    SECTION.
      *----------------------------------------------------------------*

           CALL 'POOL5005'             USING WRK-VERSAO
                                             WRK-FUNCAO
                                             LNK-IO-PCB
                                             LNK-ALT-PCB
                                             WRK-MENSAGEM
                                             WRK-TELA
                                             WRK-COD-USER
                                             WRK-COD-DEPTO.

           IF  RETURN-CODE             EQUAL 04 OR 12
               PERFORM 3000-FINALIZAR
           END-IF.

           MOVE SPACES                 TO WRK-OUT-DADOS-IUTM
                                          WRK-INCONSIS
                                          WRK-LOCAL-OPER.

           PERFORM 2000-PROCESSAR-MENSAGEM.

           GO TO 0000-INICIAR.

      *----------------------------------------------------------------*
       0000-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *     PROCESSAMENTO DA MENSAGEM RECEBIDA                         *
      *----------------------------------------------------------------*
       2000-PROCESSAR-MENSAGEM         SECTION.
      *----------------------------------------------------------------*

           MOVE 'DCOMIUTM'             TO WRK-TELA.

           IF  WRK-TRANSACAO           NOT EQUAL 'DCOM3928'
               PERFORM 2100-RECEBER-CONTROLE
           ELSE
               PERFORM 2200-PROCESSAR-DCOMIUTM
           END-IF.

           IF  WRK-TELA                EQUAL 'DCOMIUTM'
               PERFORM 2300-PROCESSAR-BRAD0660
           END-IF.

      *----------------------------------------------------------------*
       2000-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *     RECEBE CONTROLE DE OUTRA TRANSACAO IMS                     *
      *----------------------------------------------------------------*
       2100-RECEBER-CONTROLE           SECTION.
      *----------------------------------------------------------------*

           MOVE WRK-MENSAGEM           TO WRK-COMU-AREA.
           MOVE WRK-TRANSACAO          TO WRK-INP-CHAMADOR-IUTM
                                          WRK-OUT-CHAMADOR-IUTM.
           MOVE ZEROS                  TO WRK-NRO-PARCELA.

           PERFORM 2110-FORMATAR-TITULO.

           EVALUATE  WRK-TRANSACAO
               WHEN 'DCOM0630'
               WHEN 'DCOM0632'
               WHEN 'DCOM0636'
               WHEN 'DCOM0645'
                     MOVE WRK-COMU-TMSTAMP-06
                                       TO WRK-INP-TIMESTAMP-IUTM
                                          WRK-OUT-TIMESTAMP-IUTM
                     MOVE WRK-COMU-DANO-06
                                       TO WRK-INP-DANO-IUTM
                                          WRK-OUT-DANO-IUTM
                     MOVE WRK-COMU-NSEQ-06
                                       TO WRK-INP-NSEQ-IUTM
                                          WRK-OUT-NSEQ-IUTM
                     MOVE WRK-COMU-NPCELA-06
                                       TO WRK-NRO-PARCELA
                     MOVE WRK-COMU-HULT-PCELA-06
                                       TO WRK-INP-HULT-IUTM
                                          WRK-OUT-HULT-IUTM
                     MOVE WRK-COMU-OPCAO-06
                                       TO WRK-OUT-OPCAO-IUTM-N
                     MOVE WRK-COMU-HULT-BAIXA-06
                                       TO WRK-INP-HULTPARC-IUTM
                                          WRK-OUT-HULTPARC-IUTM
                     MOVE WRK-COMU-DEFETV-BAIXA-06
                                       TO WRK-INP-DEFETV-IUTM
                                          WRK-OUT-DEFETV-IUTM
                     IF  WRK-COMU-OPCAO-06 EQUAL 3 OR 1 OR 5
                         MOVE 'O'      TO WRK-LOCAL-OPER
                     ELSE
                         MOVE 'H'      TO WRK-LOCAL-OPER
                     END-IF

               WHEN 'DCOM1503'
                     MOVE WRK-COMU-OPCAO-15A   TO WRK-INP-OPCAO-IUTM
                                                  WRK-OUT-OPCAO-IUTM
                     MOVE WRK-COMU-TMSTAMP-15A
                                               TO WRK-INP-TIMESTAMP-IUTM
                                                  WRK-OUT-TIMESTAMP-IUTM
                     MOVE WRK-COMU-DANO-15A    TO WRK-INP-DANO-IUTM
                                                  WRK-OUT-DANO-IUTM
                     MOVE WRK-COMU-NSEQ-15A    TO WRK-INP-NSEQ-IUTM
                                                  WRK-OUT-NSEQ-IUTM
                     MOVE 'O'                  TO WRK-LOCAL-OPER

               WHEN 'DCOM1504'
               WHEN 'DCOM1505'
               WHEN 'DCOM1510'
               WHEN 'DCOM1512'
               WHEN 'DCOM1519'
               WHEN 'DCOM1520'
                     MOVE WRK-COMU-OPCAO-15E   TO WRK-INP-OPCAO-IUTM
                                                  WRK-OUT-OPCAO-IUTM
                     MOVE WRK-COMU-TPMANUT-15E
                                               TO WRK-INP-TMANUT-IUTM
                                                  WRK-OUT-TMANUT-IUTM
                     MOVE WRK-COMU-TMSTAMP-15E
                                               TO WRK-INP-TIMESTAMP-IUTM
                                                  WRK-OUT-TIMESTAMP-IUTM
                     MOVE WRK-COMU-LISTA-15E   TO WRK-INP-LISTA-IUTM
                                                  WRK-OUT-LISTA-IUTM
                     MOVE WRK-COMU-DANO-15E    TO WRK-INP-DANO-IUTM
                                                  WRK-OUT-DANO-IUTM
                     MOVE WRK-COMU-NSEQ-15E    TO WRK-INP-NSEQ-IUTM
                                                  WRK-OUT-NSEQ-IUTM
                     MOVE WRK-COMU-PCELA-15E   TO WRK-NRO-PARCELA
                     MOVE WRK-COMU-PGMANT-15E TO WRK-INP-PGMANT-IUTM
                                                  WRK-OUT-PGMANT-IUTM
                     MOVE 'O'                  TO WRK-LOCAL-OPER

               WHEN 'DCOM1506'
               WHEN 'DCOM1508'
               WHEN 'DCOM1518'
DEISI          WHEN 'DCOM1527'
                     MOVE WRK-COMU-OPCAO-15C   TO WRK-INP-OPCAO-IUTM
                                                  WRK-OUT-OPCAO-IUTM
                     MOVE WRK-COMU-TMSTAMP-15C
                                               TO WRK-INP-TIMESTAMP-IUTM
                                                  WRK-OUT-TIMESTAMP-IUTM
                     MOVE WRK-COMU-DANO-15C    TO WRK-INP-DANO-IUTM
                                                  WRK-OUT-DANO-IUTM
                     MOVE WRK-COMU-NSEQ-15C    TO WRK-INP-NSEQ-IUTM
                                                  WRK-OUT-NSEQ-IUTM
                     MOVE WRK-COMU-PCELA-15C   TO WRK-NRO-PARCELA
                     MOVE 'O'                  TO WRK-LOCAL-OPER

               WHEN 'DCOM1705'
               WHEN 'DCOM1706'
               WHEN 'DCOM1707'
               WHEN 'DCOM1708'
               WHEN 'DCOM1709'
               WHEN 'DCOM1710'
               WHEN 'DCOM1711'
               WHEN 'DCOM1712'
               WHEN 'DCOM1713'
                     MOVE WRK-COMU-OPCAO-17    TO WRK-INP-OPCAO-IUTM-N
                                                  WRK-OUT-OPCAO-IUTM-N
                     MOVE WRK-COMU-TMSTAMP-17  TO WRK-INP-TIMESTAMP-IUTM
                                                  WRK-OUT-TIMESTAMP-IUTM
                     MOVE WRK-COMU-DANO-17     TO WRK-INP-DANO-IUTM
                                                  WRK-OUT-DANO-IUTM
                     MOVE WRK-COMU-NSEQ-17     TO WRK-INP-NSEQ-IUTM
                                                  WRK-OUT-NSEQ-IUTM
                     MOVE WRK-COMU-NPCELA-17   TO WRK-NRO-PARCELA
                     MOVE WRK-COMU-OUTROS1-17  TO WRK-INP-AREA1-IUTM
                                                  WRK-OUT-AREA1-IUTM
                     MOVE WRK-COMU-OUTROS2-17  TO WRK-INP-AREA2-IUTM
                                                  WRK-OUT-AREA2-IUTM
                     MOVE WRK-COMU-CHAMADOR-17 TO WRK-INP-PGMANT-IUTM
                                                  WRK-OUT-PGMANT-IUTM
                     MOVE 'O'                  TO WRK-LOCAL-OPER

               WHEN 'DCOM2405'
               WHEN 'DCOM2408'
                     MOVE WRK-COMU-OPCAO-24    TO WRK-INP-OPCAO-IUTM-N
                                                  WRK-OUT-OPCAO-IUTM-N
                     MOVE WRK-COMU-TMSTAMP-24  TO WRK-INP-TIMESTAMP-IUTM
                                                  WRK-OUT-TIMESTAMP-IUTM
                     MOVE WRK-COMU-DANO-24     TO WRK-INP-DANO-IUTM
                                                  WRK-OUT-DANO-IUTM
                     MOVE WRK-COMU-NSEQ-24     TO WRK-INP-NSEQ-IUTM
                                                  WRK-OUT-NSEQ-IUTM
                     MOVE WRK-COMU-NPCELA-24   TO WRK-NRO-PARCELA
                     IF WRK-NRO-PARCELA        GREATER ZEROS
                        MOVE WRK-CABEC-PARCELA TO WRK-INP-DETPARC-IUTM
                                                  WRK-OUT-DETPARC-IUTM
                     END-IF
                     MOVE WRK-COMU-AREA-24     TO WRK-INP-AREA-IUTM
                                                  WRK-OUT-AREA-IUTM
                     MOVE 'R'                  TO WRK-LOCAL-OPER
           END-EVALUATE.

           PERFORM 2120-OBTER-OPERACAO.

           PERFORM 2130-OBTER-JUROS-MORATORIOS.

           MOVE WRK-MSG001             TO WRK-OUT-MENSA-IUTM.

      *----------------------------------------------------------------*
       2100-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *     OBTER DADOS DA OPERACAO EM BASE OFICIAL                    *
      *----------------------------------------------------------------*
       2110-FORMATAR-TITULO            SECTION.
      *----------------------------------------------------------------*

           EVALUATE  WRK-TRANSACAO
               WHEN 'DCOM1503'
               MOVE '   INCLUIR BAIXA DE OPERACAO - JUROS E MULTAS   '
                                       TO WRK-OUT-TITULO-IUTM

               WHEN 'DCOM1504'
               MOVE '   EXCLUIR BAIXA DE OPERACAO - JUROS E MULTAS   '
                                       TO WRK-OUT-TITULO-IUTM

               WHEN 'DCOM1505'
               MOVE '  CONSULTAR BAIXA DE OPERACAO - JUROS E MULTAS  '
                                       TO WRK-OUT-TITULO-IUTM

               WHEN 'DCOM1506'
               MOVE 'INCLUIR BAIXA PARCELA - TITULOS - JUROS E MULTAS'
                                       TO WRK-OUT-TITULO-IUTM

               WHEN 'DCOM1508'
               MOVE 'EXCLUIR BAIXA PARCELA - TITULOS - JUROS E MULTAS'
                                       TO WRK-OUT-TITULO-IUTM

               WHEN 'DCOM1510'
               MOVE 'CONSULTAR BAIXA PARCELA - TITULOS - JUROS MULTAS'
                                       TO WRK-OUT-TITULO-IUTM

               WHEN 'DCOM1512'
               MOVE 'INCLUIR BAIXA PARCELA POR RATEIO - JUROS MULTAS '
                                       TO WRK-OUT-TITULO-IUTM

DEISI          WHEN 'DCOM1527'
DEISI          MOVE 'INCLUIR BAIXA DE PARCELA (BOLETOS) - TITULOS'
DEISI                                  TO WRK-OUT-TITULO-IUTM

               WHEN 'DCOM1518'
               MOVE 'INCLUIR BAIXA PARCELA - CHEQUES - JUROS E MULTAS'
                                       TO WRK-OUT-TITULO-IUTM

               WHEN 'DCOM1519'
               MOVE 'EXCLUIR BAIXA PARCELA - CHEQUES - JUROS E MULTAS'
                                       TO WRK-OUT-TITULO-IUTM

               WHEN 'DCOM1520'
               MOVE 'CONSULTAR BAIXA PARCELA - CHEQUES - JUROS MULTAS'
                                       TO WRK-OUT-TITULO-IUTM

               WHEN 'DCOM1705'
               MOVE 'INCLUIR ESTORNO BAIXA DA OPERACAO - JUROS MULTAS'
                                       TO WRK-OUT-TITULO-IUTM

               WHEN 'DCOM1706'
               MOVE 'EXCLUIR ESTORNO BAIXA DA OPERACAO - JUROS MULTAS'
                                       TO WRK-OUT-TITULO-IUTM

               WHEN 'DCOM1707'
               MOVE 'CONSULTAR ESTORNO BAIXA OPERACAO - JUROS MULTAS '
                                       TO WRK-OUT-TITULO-IUTM

               WHEN 'DCOM1708'
               WHEN 'DCOM1709'
               MOVE ' INCLUIR ESTORNO BAIXA PARCELA - JUROS E MULTAS '
                                       TO WRK-OUT-TITULO-IUTM

               WHEN 'DCOM1710'
               WHEN 'DCOM1711'
               MOVE ' EXCLUIR ESTORNO BAIXA PARCELA - JUROS E MULTAS '
                                       TO WRK-OUT-TITULO-IUTM

               WHEN 'DCOM1712'
               WHEN 'DCOM1713'
               MOVE 'CONSULTAR ESTORNO BAIXA PARCELA - JUROS E MULTAS'
                                       TO WRK-OUT-TITULO-IUTM

               WHEN 'DCOM2405'
               MOVE ' REATIVACAO PARCELAS - TITULOS - JUROS E MULTAS '
                                       TO WRK-OUT-TITULO-IUTM

               WHEN 'DCOM2408'
               MOVE ' REATIVACAO PARCELAS - CHEQUES - JUROS E MULTAS '
                                       TO WRK-OUT-TITULO-IUTM

               WHEN 'DCOM0630'
               MOVE '  CONSULTA PARCELAS - TITULOS - JUROS E MULTAS  '
                                       TO WRK-OUT-TITULO-IUTM

               WHEN 'DCOM0632'
               MOVE '  CONSULTA PARCELAS - CHEQUES - JUROS E MULTAS  '
                                       TO WRK-OUT-TITULO-IUTM

               WHEN 'DCOM0636'
               MOVE ' CONSULTA HISTORICO DA PARCELA - JUROS E MULTAS '
                                       TO WRK-OUT-TITULO-IUTM

               WHEN 'DCOM0645'
               MOVE '      CONSULTA PAGAMENTOS - JUROS E MULTAS      '
                                       TO WRK-OUT-TITULO-IUTM
           END-EVALUATE.

      *----------------------------------------------------------------*
       2110-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *  OBTER OPERACAO DE ACORDO COM O LOCAL                          *
      *----------------------------------------------------------------*
       2120-OBTER-OPERACAO             SECTION.
      *----------------------------------------------------------------*

           INITIALIZE 5537-ENTRADA-ROTEADOR
                       5537-SAIDA-ROTEADOR.

           MOVE WRK-COD-USER-R         TO 5537-ENT-CFUNC-BDSCO.
           MOVE LNK-IO-LTERM           TO 5537-ENT-CTERM.
           MOVE WRK-LOCAL-OPER         TO 5537-ENT-FLAG.
           MOVE WRK-INP-DANO-IUTM      TO 5537-ENT-DANO-OPER-DESC.
           MOVE WRK-INP-NSEQ-IUTM      TO 5537-ENT-NSEQ-OPER-DESC.
           MOVE WRK-INP-HULT-IUTM      TO 5537-ENT-HULT-ATULZ.
           MOVE 'DCOM5537'             TO WRK-MODULO.

           CALL WRK-MODULO             USING 5537-ENTRADA-ROTEADOR
                                             5537-SAIDA-ROTEADOR
                                             ERRO-AREA
                                             WRK-SQLCA.

           EVALUATE  5537-SAI-COD-RETORNO
               WHEN '0000'
                     PERFORM 2121-FORMATAR-TELA-5537

               WHEN '0099'
                     MOVE 'DB2'        TO ERR-TIPO-ACESSO
                     PERFORM 9999-PROCESSAR-ROTINA-ERRO

               WHEN  OTHER
                     MOVE '*'          TO WRK-INCONSIS
                     MOVE 5537-SAI-MSG-RETORNO
                                       TO WRK-OUT-MENSA-IUTM
                     PERFORM 2122-RETORNAR-CHAMADOR
           END-EVALUATE.

      *----------------------------------------------------------------*
       2120-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *  ESSA ROTINA CARREGA OS CAMPOS DA TELA                         *
      *----------------------------------------------------------------*
       2121-FORMATAR-TELA-5537         SECTION.
      *----------------------------------------------------------------*

           MOVE 5537-SAI-FLAG          TO WRK-LOCAL-OPER.
           MOVE 5537-SAI-IPSSOA-DESC-COML
                                (1:40) TO WRK-INP-NOME-IUTM
                                          WRK-OUT-NOME-IUTM

4S2511*        MOVE 5537-SAI-CCNPJ-CPF     TO WRK-INP-CNPJOC-IUTM
4S2511*                                           WRK-OUT-CNPJOC-IUTM.
4S2511*        MOVE 5537-SAI-CCNPJ-CPF     TO WRK-NRO-CNPJCPF.
4S2511*        MOVE 5537-SAI-CFLIAL-CNPJ   TO WRK-FIL-CNPJ.
4S2511*        MOVE 5537-SAI-CCTRL-CNPJ-CPF
4S2511*                                    TO WRK-CTR-CNPJCPF.
4S2511*        MOVE WRK-CNPJCPF            TO WRK-OUT-CNPJCPF-IUTM.
4S2511         IF 5537-SAI-CCNPJ-CPF    EQUAL SPACES OR
4S2511            5537-SAI-CCNPJ-CPF    EQUAL LOW-VALUES OR
4S2511            5537-SAI-CFLIAL-CNPJ  EQUAL SPACES OR
4S2511            5537-SAI-CFLIAL-CNPJ  EQUAL LOW-VALUES
4S2511             MOVE SPACES          TO WRK-INP-CNPJOC-IUTM
4S2511             MOVE SPACES          TO WRK-OUT-CNPJOC-IUTM
4S2511             MOVE SPACES          TO WRK-NRO-CNPJCPF
4S2511             MOVE SPACES          TO WRK-FIL-CNPJ
4S2511             MOVE ZEROS           TO WRK-CTR-CNPJCPF
4S2511             MOVE SPACES          TO WRK-OUT-CNPJCPF-IUTM
4S2511         ELSE
4S2511             MOVE 5537-SAI-CCNPJ-CPF     TO WRK-INP-CNPJOC-IUTM
4S2511                                             WRK-OUT-CNPJOC-IUTM
4S2511             MOVE 5537-SAI-CCNPJ-CPF     TO WRK-NRO-CNPJCPF
4S2511             MOVE 5537-SAI-CFLIAL-CNPJ   TO WRK-FIL-CNPJ
4S2511             MOVE 5537-SAI-CCTRL-CNPJ-CPF TO WRK-CTR-CNPJCPF
4S2511             MOVE WRK-CNPJCPF            TO WRK-OUT-CNPJCPF-IUTM
4S2511         END-IF

           MOVE 5537-SAI-CBCO          TO WRK-BCO.
           MOVE 5537-SAI-CAG-BCRIA     TO WRK-AGE.
           MOVE 5537-SAI-CCTA-BCRIA-CLI
                                       TO WRK-CTA.
           MOVE WRK-BCOAGECTA          TO WRK-OUT-BCAGCTA-IUTM.

           MOVE 5537-SAI-CPRODT        TO WRK-INP-CPRODT-IUTM
                                          WRK-OUT-CPRODT-IUTM
           MOVE 5537-SAI-IABREV-PRODT  TO WRK-INP-IPRODT-IUTM
                                          WRK-OUT-IPRODT-IUTM
           MOVE 5537-SAI-CSPROD-DESC-COML
                                       TO WRK-INP-CSPROD-IUTM
                                          WRK-OUT-CSPROD-IUTM
           MOVE 5537-SAI-IRSUMO-SPROD-DESC
                                       TO WRK-INP-ISPROD-IUTM
                                          WRK-OUT-ISPROD-IUTM
           MOVE 5537-SAI-IRSUMO-SIT-DESC
                                       TO WRK-INP-SITUAC-IUTM
                                          WRK-OUT-SITUAC-IUTM

           IF 5537-SAI-CELMTO-DESC-COML
                                       EQUAL 08
              MOVE 'CONV. GERAL.:'     TO WRK-OUT-DESIDE1-IUTM
              MOVE 5537-SAI-CCONTR-CONVE-DESC
                                       TO WRK-OUT-NUMERO1-IUTM-N
           END-IF.

           IF 5537-SAI-CELMTO-DESC-COML
                                       EQUAL 09
              MOVE 'CONV.CLIENTE:'     TO WRK-OUT-DESIDE1-IUTM
              MOVE 5537-SAI-CCONTR-CONVE-DESC
                                       TO WRK-OUT-NUMERO1-IUTM-N
           END-IF.

           IF 5537-SAI-CELMTO-DESC-COML
                                       EQUAL 10
              MOVE 'CONTR.LIMITE:'     TO WRK-OUT-DESIDE1-IUTM
              MOVE 5537-SAI-CCONTR-LIM-DESC
                                       TO WRK-OUT-NUMERO1-IUTM-N
           END-IF.

           IF  5537-SAI-NSMULA-OPER-DESC GREATER ZEROS
               IF  WRK-OUT-DESIDE1-IUTM  EQUAL SPACES OR LOW-VALUES
                   MOVE 'SIMULACAO...:'  TO WRK-OUT-DESIDE1-IUTM
                   MOVE 5537-SAI-NSMULA-OPER-DESC
                                         TO WRK-OUT-NUMERO1-IUTM-N
               ELSE
                   MOVE 'SIMULACAO...:' TO WRK-OUT-DESIDE2-IUTM
                   MOVE 5537-SAI-NSMULA-OPER-DESC
                                         TO WRK-OUT-NUMERO2-IUTM-N
               END-IF
           END-IF.

      *----------------------------------------------------------------*
       2121-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *     DEVOLVE CONTROLE AO PROGRAMA CHAMADOR                      *
      *----------------------------------------------------------------*
       2122-RETORNAR-CHAMADOR          SECTION.
      *----------------------------------------------------------------*

           INITIALIZE WRK-COMU-AREA.

           MOVE LENGTH OF WRK-COMU-AREA      TO WRK-COMU-LL.
           MOVE 'DCOM3928'                   TO WRK-COMU-TRANSACAO.

           EVALUATE  WRK-INP-CHAMADOR-IUTM
               WHEN 'DCOM0630'
               WHEN 'DCOM0632'
               WHEN 'DCOM0636'
               WHEN 'DCOM0645'
                     INITIALIZE WRK-COMU-DADOS-06
                     MOVE WRK-INP-TIMESTAMP-IUTM
                                             TO WRK-COMU-TMSTAMP-06
                     MOVE WRK-INP-DANO-IUTM  TO WRK-COMU-DANO-06
                     MOVE WRK-INP-NSEQ-IUTM  TO WRK-COMU-NSEQ-06
                     MOVE WRK-INP-HULT-IUTM  TO WRK-COMU-HULT-PCELA-06
                     MOVE WRK-INP-OPCAO-IUTM-N
                                             TO WRK-COMU-OPCAO-06

                     IF WRK-INP-DETPARC-IUTM EQUAL SPACES
                        MOVE ZEROS           TO WRK-COMU-NPCELA-06
                     ELSE
                        MOVE WRK-INP-DETPARC-IUTM
                                             TO WRK-CABEC-PARCELA
                        MOVE WRK-NRO-PARCELA TO WRK-COMU-NPCELA-06
                     END-IF

                     IF  WRK-INCONSIS        EQUAL '*'
                         MOVE WRK-OUT-MENSA-IUTM
                                             TO WRK-COMU-MENSA-06
                     END-IF

               WHEN 'DCOM1503'
                     INITIALIZE WRK-COMU-DADOS-15A
                     MOVE WRK-INP-OPCAO-IUTM TO WRK-COMU-OPCAO-15A
                     MOVE WRK-INP-TIMESTAMP-IUTM
                                             TO WRK-COMU-TMSTAMP-15A
                     MOVE WRK-INP-DANO-IUTM  TO WRK-COMU-DANO-15A
                     MOVE WRK-INP-NSEQ-IUTM  TO WRK-COMU-NSEQ-15A

                     IF  WRK-INCONSIS        EQUAL '*'
                         MOVE WRK-OUT-MENSA-IUTM
                                             TO WRK-COMU-MENSA-15A
                     END-IF

               WHEN 'DCOM1504'
               WHEN 'DCOM1505'
               WHEN 'DCOM1510'
               WHEN 'DCOM1512'
               WHEN 'DCOM1519'
               WHEN 'DCOM1520'
                     INITIALIZE WRK-COMU-DADOS-15E
                     MOVE WRK-INP-OPCAO-IUTM TO WRK-COMU-OPCAO-15E
                     MOVE WRK-INP-TMANUT-IUTM
                                             TO WRK-COMU-TPMANUT-15E
                     MOVE WRK-INP-TIMESTAMP-IUTM
                                             TO WRK-COMU-TMSTAMP-15E
                     MOVE WRK-INP-LISTA-IUTM TO WRK-COMU-LISTA-15E
                     MOVE WRK-INP-DANO-IUTM  TO WRK-COMU-DANO-15E
                     MOVE WRK-INP-NSEQ-IUTM  TO WRK-COMU-NSEQ-15E
                     MOVE WRK-INP-PGMANT-IUTM
                                             TO WRK-COMU-PGMANT-15E

                     IF WRK-INP-DETPARC-IUTM EQUAL SPACES
                        MOVE ZEROS           TO WRK-COMU-PCELA-15E
                     ELSE
                        MOVE WRK-INP-DETPARC-IUTM
                                             TO WRK-CABEC-PARCELA
                        MOVE WRK-NRO-PARCELA TO WRK-COMU-PCELA-15E
                     END-IF

                     IF  WRK-INCONSIS        EQUAL '*'
                         MOVE WRK-OUT-MENSA-IUTM
                                             TO WRK-COMU-MENSA-15E
                     END-IF

               WHEN 'DCOM1506'
               WHEN 'DCOM1508'
               WHEN 'DCOM1518'
DEISI          WHEN 'DCOM1527'
                     INITIALIZE WRK-COMU-DADOS-15C
                     MOVE WRK-INP-OPCAO-IUTM TO WRK-COMU-OPCAO-15C
                     MOVE WRK-INP-TIMESTAMP-IUTM
                                             TO WRK-COMU-TMSTAMP-15C
                     MOVE WRK-INP-DANO-IUTM  TO WRK-COMU-DANO-15C
                     MOVE WRK-INP-NSEQ-IUTM  TO WRK-COMU-NSEQ-15C
                     MOVE WRK-INP-DETPARC-IUTM
                                             TO WRK-CABEC-PARCELA
                     MOVE WRK-NRO-PARCELA    TO WRK-COMU-PCELA-15C

                     IF  WRK-INCONSIS        EQUAL '*'
                         MOVE WRK-OUT-MENSA-IUTM
                                             TO WRK-COMU-MENSA-15C
                     END-IF

               WHEN 'DCOM1705'
               WHEN 'DCOM1706'
               WHEN 'DCOM1707'
               WHEN 'DCOM1708'
               WHEN 'DCOM1709'
               WHEN 'DCOM1710'
               WHEN 'DCOM1711'
               WHEN 'DCOM1712'
               WHEN 'DCOM1713'
                     INITIALIZE WRK-COMU-DADOS-17
                     MOVE WRK-INP-OPCAO-IUTM-N
                                             TO WRK-COMU-OPCAO-17
                     MOVE WRK-INP-TIMESTAMP-IUTM
                                             TO WRK-COMU-TMSTAMP-17
                     MOVE WRK-INP-DANO-IUTM  TO WRK-COMU-DANO-17
                     MOVE WRK-INP-NSEQ-IUTM  TO WRK-COMU-NSEQ-17
                     MOVE WRK-INP-DETPARC-IUTM
                                             TO WRK-CABEC-PARCELA
                     MOVE WRK-NRO-PARCELA    TO WRK-COMU-NPCELA-17
                     MOVE WRK-INP-AREA1-IUTM TO WRK-COMU-OUTROS1-17
                     MOVE WRK-INP-AREA2-IUTM TO WRK-COMU-OUTROS2-17
                     MOVE WRK-INP-PGMANT-IUTM
                                             TO WRK-COMU-CHAMADOR-17
                     IF  WRK-INCONSIS        EQUAL '*'
                         MOVE WRK-OUT-MENSA-IUTM
                                             TO WRK-COMU-MENSA-17
                     END-IF

               WHEN 'DCOM2405'
               WHEN 'DCOM2408'
                     MOVE WRK-INP-OPCAO-IUTM-N TO WRK-COMU-OPCAO-24
                     MOVE WRK-INP-TIMESTAMP-IUTM
                                               TO WRK-COMU-TMSTAMP-24
                     MOVE WRK-INP-DANO-IUTM    TO WRK-COMU-DANO-24
                     MOVE WRK-INP-NSEQ-IUTM    TO WRK-COMU-NSEQ-24

                     MOVE WRK-INP-AREA-IUTM   (1:LENGTH
                                               OF WRK-COMU-AREA-24)
                                               TO WRK-COMU-AREA-24
                     MOVE WRK-INP-DETPARC-IUTM
                                             TO WRK-CABEC-PARCELA
                     MOVE WRK-NRO-PARCELA    TO WRK-COMU-NPCELA-24

                     IF  WRK-INCONSIS        EQUAL '*'
                         MOVE WRK-OUT-MENSA-IUTM
                                             TO WRK-COMU-MENSA-24
                     END-IF
           END-EVALUATE.

           MOVE WRK-COMU-AREA                TO WRK-MENSAGEM.
           MOVE WRK-INP-CHAMADOR-IUTM        TO WRK-TELA.
           MOVE WRK-CHNG                     TO WRK-FUNCAO.

      *----------------------------------------------------------------*
       2122-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *     OBTER ENCARGOS MORATORIOS                                  *
      *----------------------------------------------------------------*
       2130-OBTER-JUROS-MORATORIOS     SECTION.
      *----------------------------------------------------------------*

           INITIALIZE WRK-5169-ENTRADA
                      WRK-5169-SAIDA.

           MOVE WRK-COD-USER-R         TO WRK-5169E-CFUNC-BDSCO.
           MOVE LNK-IO-LTERM           TO WRK-5169E-CTERM.
           MOVE WRK-COD-DEPTO-N        TO WRK-5169E-AGENCIA-FUNC.
           MOVE WRK-INP-DANO-IUTM      TO WRK-5169E-DANO-OPER-DESC.
           MOVE WRK-INP-NSEQ-IUTM      TO WRK-5169E-NSEQ-OPER-DESC.
           MOVE WRK-NRO-PARCELA        TO WRK-5169E-NPCELA-DESC-COML.
           MOVE WRK-INP-HULT-IUTM      TO WRK-5169E-HULT-ATULZ.
           MOVE WRK-INP-HULTPARC-IUTM  TO WRK-5169E-HBAIXA-PCELA.
           MOVE WRK-INP-DEFETV-IUTM    TO WRK-5169E-DEFETV-BAIXA.
           MOVE WRK-LOCAL-OPER         TO WRK-5169E-LOCAL.
           MOVE WRK-INP-OPCAO-IUTM-N   TO WRK-5169E-OPCAO-MENU.
4S2511*        MOVE WRK-INP-CNPJOC-IUTM    TO WRK-5169E-CCNPJ-CPF.
4S2511*        MOVE WRK-OUT-CNPJCPF-IUTM   TO WRK-CNPJCPF.
4S2511*        MOVE WRK-FIL-CNPJ           TO WRK-5169E-CFLIAL-CNPJ
4S2511*        MOVE WRK-CTR-CNPJCPF        TO WRK-5169E-CCTRL-CNPJ-CPF.
4S2511         IF WRK-INP-CNPJOC-IUTM  EQUAL SPACES OR
4S2511            WRK-INP-CNPJOC-IUTM  EQUAL LOW-VALUES
4S2511             MOVE SPACES         TO WRK-5169E-CCNPJ-CPF
4S2511             MOVE SPACES         TO WRK-5169E-CFLIAL-CNPJ
4S2511             MOVE ZEROS          TO WRK-5169E-CCTRL-CNPJ-CPF
4S2511         ELSE
4S2511             MOVE WRK-INP-CNPJOC-IUTM    TO WRK-5169E-CCNPJ-CPF
4S2511             MOVE WRK-FIL-CNPJ           TO WRK-5169E-CFLIAL-CNPJ
4S2511             MOVE WRK-CTR-CNPJCPF TO WRK-5169E-CCTRL-CNPJ-CPF
4S2511         END-IF

           EVALUATE  WRK-INP-CHAMADOR-IUTM
               WHEN 'DCOM0630'
               WHEN 'DCOM0632'
               WHEN 'DCOM0636'
                     MOVE 06           TO WRK-5169E-OPCAO-SENHAS
                     MOVE 'C '         TO WRK-5169E-FUNCAO

               WHEN 'DCOM0645'
                     MOVE 06           TO WRK-5169E-OPCAO-SENHAS
                     MOVE 'B '         TO WRK-5169E-FUNCAO

               WHEN 'DCOM1503'
               WHEN 'DCOM1504'
               WHEN 'DCOM1505'
                     MOVE 15           TO WRK-5169E-OPCAO-SENHAS
                     MOVE 'O '         TO WRK-5169E-FUNCAO

               WHEN 'DCOM1506'
               WHEN 'DCOM1508'
               WHEN 'DCOM1510'
               WHEN 'DCOM1518'
DEISI          WHEN 'DCOM1527'
               WHEN 'DCOM1519'
               WHEN 'DCOM1520'
                     MOVE 15           TO WRK-5169E-OPCAO-SENHAS
                     MOVE 'P '         TO WRK-5169E-FUNCAO

               WHEN 'DCOM1512'
                     MOVE 15           TO WRK-5169E-OPCAO-SENHAS
                     MOVE 'T '         TO WRK-5169E-FUNCAO

               WHEN 'DCOM1705'
               WHEN 'DCOM1706'
               WHEN 'DCOM1707'
               WHEN 'DCOM1708'
               WHEN 'DCOM1709'
               WHEN 'DCOM1710'
               WHEN 'DCOM1711'
               WHEN 'DCOM1712'
               WHEN 'DCOM1713'
                     MOVE 17           TO WRK-5169E-OPCAO-SENHAS
                     MOVE 'S '         TO WRK-5169E-FUNCAO

               WHEN 'DCOM2405'
                     MOVE 24           TO WRK-5169E-OPCAO-SENHAS
                     MOVE 'R '         TO WRK-5169E-FUNCAO

               WHEN 'DCOM2408'
                     MOVE 24           TO WRK-5169E-OPCAO-SENHAS
                     MOVE WRK-OUT-VPRINC-IUTM-N
                                       TO WRK-5169E-VLR-RECALCULO

                     IF  WRK-INP-CHAMADOR3-IUTM
                                       EQUAL 'DCOM2410'
                         MOVE 'RP'     TO WRK-5169E-FUNCAO
                     ELSE
                         MOVE 'RT'     TO WRK-5169E-FUNCAO
                     END-IF
           END-EVALUATE.

           MOVE 'DCOM5169'             TO WRK-MODULO.

           CALL WRK-MODULO             USING WRK-5169-ENTRADA
                                             WRK-5169-SAIDA
                                             ERRO-AREA
                                             WRK-SQLCA.

           EVALUATE  WRK-5169S-COD-RETORNO
               WHEN  ZEROS
                     PERFORM 2131-FORMATAR-ENCARGOS

               WHEN  0099
                     MOVE 'DB2'        TO ERR-TIPO-ACESSO
                     PERFORM 9999-PROCESSAR-ROTINA-ERRO

               WHEN  OTHER
                     MOVE '*'          TO WRK-INCONSIS
                     MOVE WRK-5169S-MSG-RETORNO
                                       TO WRK-OUT-MENSA-IUTM
                     PERFORM 2122-RETORNAR-CHAMADOR
           END-EVALUATE.

      *----------------------------------------------------------------*
       2130-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *     FORMATAR ENCARGOS MORATORIOS NA TELA DCOMIUTM              *
      *----------------------------------------------------------------*
       2131-FORMATAR-ENCARGOS          SECTION.
      *----------------------------------------------------------------*

           MOVE WRK-5169S-VJURO-REMUN-VENCD
                                       TO WRK-OUT-VJREMUN-IUTM-N.
           MOVE WRK-5169S-PJURO-REMUN-VENCD
                                       TO WRK-OUT-PJREMUN-IUTM-N.
           MOVE WRK-5169S-VMORA-VENCD-DESC
                                       TO WRK-OUT-VJMORAT-IUTM-N.
           MOVE WRK-5169S-PMORA-VENCD-DESC
                                       TO WRK-OUT-PJMORAT-IUTM-N.
           MOVE WRK-5169S-VMULTA-VENCD-DESC
                                       TO WRK-OUT-VJMULTA-IUTM-N.
           MOVE WRK-5169S-PMULTA-VENCD-DESC
                                       TO WRK-OUT-PJMULTA-IUTM-N.

           IF  WRK-NRO-PARCELA         EQUAL ZEROS
               MOVE SPACES             TO WRK-INP-DETPARC-IUTM
                                          WRK-OUT-DETPARC-IUTM
           ELSE
               MOVE WRK-5169S-ISIT-DESC-COML(1:16)
                                       TO WRK-SIT-PARCELA
               MOVE WRK-CABEC-PARCELA  TO WRK-INP-DETPARC-IUTM
                                          WRK-OUT-DETPARC-IUTM
           END-IF.

      *----------------------------------------------------------------*
       2131-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *     PROCESSAMENTO DA MENSAGEM ENVIADA PELA TELA DCOMIUTM       *
      *----------------------------------------------------------------*
       2200-PROCESSAR-DCOMIUTM         SECTION.
      *----------------------------------------------------------------*

           MOVE WRK-MENSAGEM                TO WRK-INPUT-IUTM.

           PERFORM 2210-DEVOLVER-TELA.

           EVALUATE TRUE
               WHEN WRK-INP-PFK-IUTM        EQUAL 'H' OR
                   (WRK-INP-PFK-IUTM        EQUAL '.' AND
                    WRK-INP-COMANDO-IUTM    EQUAL 'PFK01')
                    MOVE WRK-INP-MENSA-IUTM TO WRK-OUT-MENSA-IUTM

               WHEN WRK-INP-PFK-IUTM        EQUAL '3' OR
                   (WRK-INP-PFK-IUTM        EQUAL '.' AND
                    WRK-INP-COMANDO-IUTM    EQUAL 'PFK03')
                    PERFORM 2122-RETORNAR-CHAMADOR

               WHEN WRK-INP-PFK-IUTM        EQUAL '5' OR
                   (WRK-INP-PFK-IUTM        EQUAL '.' AND
                    WRK-INP-COMANDO-IUTM    EQUAL 'PFK05')
                    PERFORM 2220-RETORNAR-MENU-ROTINAS

               WHEN WRK-INP-PFK-IUTM        EQUAL 'A' OR
                   (WRK-INP-PFK-IUTM        EQUAL '.' AND
                    WRK-INP-COMANDO-IUTM    EQUAL 'PFK10')
                    PERFORM 2240-RETORNAR-MENU-DCOM

               WHEN WRK-INP-PFK-IUTM        EQUAL '.' AND
                    WRK-INP-COMANDO-IUTM    EQUAL SPACES
                    MOVE WRK-MSG001         TO WRK-OUT-MENSA-IUTM

               WHEN OTHER
                    MOVE WRK-MSG002         TO WRK-OUT-MENSA-IUTM
           END-EVALUATE.

      *----------------------------------------------------------------*
       2200-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *     MANTEM TELA DEVOLVENDO DADOS DE INPUT PARA OUTPUT          *
      *----------------------------------------------------------------*
       2210-DEVOLVER-TELA              SECTION.
      *----------------------------------------------------------------*

           MOVE WRK-INP-TITULO-IUTM    TO WRK-OUT-TITULO-IUTM
           MOVE WRK-INP-NOME-IUTM      TO WRK-OUT-NOME-IUTM.
           MOVE WRK-INP-CNPJCPF-IUTM   TO WRK-OUT-CNPJCPF-IUTM.
           MOVE WRK-INP-BCAGCTA-IUTM   TO WRK-OUT-BCAGCTA-IUTM.
           MOVE WRK-INP-CPRODT-IUTM    TO WRK-OUT-CPRODT-IUTM.
           MOVE WRK-INP-IPRODT-IUTM    TO WRK-OUT-IPRODT-IUTM.
           MOVE WRK-INP-CSPROD-IUTM    TO WRK-OUT-CSPROD-IUTM.
           MOVE WRK-INP-ISPROD-IUTM    TO WRK-OUT-ISPROD-IUTM.
           MOVE WRK-INP-OPERAC-IUTM    TO WRK-OUT-OPERAC-IUTM.
           MOVE WRK-INP-SITUAC-IUTM    TO WRK-OUT-SITUAC-IUTM.
           MOVE WRK-INP-DESIDE1-IUTM   TO WRK-OUT-DESIDE1-IUTM.
           MOVE WRK-INP-NUMERO1-IUTM   TO WRK-OUT-NUMERO1-IUTM.
           MOVE WRK-INP-DESIDE2-IUTM   TO WRK-OUT-DESIDE2-IUTM.
           MOVE WRK-INP-NUMERO2-IUTM   TO WRK-OUT-NUMERO2-IUTM.
           MOVE WRK-INP-DETPARC-IUTM   TO WRK-OUT-DETPARC-IUTM.
           MOVE WRK-INP-OCULTOS-IUTM   TO WRK-OUT-OCULTOS-IUTM.
           MOVE WRK-INP-VJREMUN-IUTM   TO WRK-OUT-VJREMUN-IUTM.
           MOVE WRK-INP-PJREMUN-IUTM   TO WRK-OUT-PJREMUN-IUTM.
           MOVE WRK-INP-VJMORAT-IUTM   TO WRK-OUT-VJMORAT-IUTM.
           MOVE WRK-INP-PJMORAT-IUTM   TO WRK-OUT-PJMORAT-IUTM.
           MOVE WRK-INP-VJMULTA-IUTM   TO WRK-OUT-VJMULTA-IUTM.
           MOVE WRK-INP-PJMULTA-IUTM   TO WRK-OUT-PJMULTA-IUTM.

      *----------------------------------------------------------------*
       2210-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *     RETORNA AO MENU DE ROTINAS DO SENHAS                       *
      *----------------------------------------------------------------*
       2220-RETORNAR-MENU-ROTINAS      SECTION.
      *----------------------------------------------------------------*

           INITIALIZE WRK-COMU-AREA.

           MOVE LENGTH OF WRK-COMU-AREA TO WRK-COMU-LL.
           MOVE WRK-CHNG                TO WRK-FUNCAO.
           MOVE 'DCOM3928'              TO WRK-TELA
                                           WRK-COMU-TRANSACAO.
           MOVE '5'                     TO WRK-COMU-PFK.
           MOVE WRK-COMU-AREA           TO WRK-MENSAGEM.

      *----------------------------------------------------------------*
       2220-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *     RETORNA AO MENU DCOM                                       *
      *----------------------------------------------------------------*
       2240-RETORNAR-MENU-DCOM         SECTION.
      *----------------------------------------------------------------*

           INITIALIZE WRK-COMU-AREA.

           MOVE LENGTH OF WRK-COMU-AREA TO WRK-COMU-LL.
           MOVE WRK-CHNG                TO WRK-FUNCAO.
           MOVE 'DCOM3928'              TO WRK-TELA
                                           WRK-COMU-TRANSACAO.
           MOVE 'A'                     TO WRK-COMU-PFK.
           MOVE WRK-COMU-AREA           TO WRK-MENSAGEM.

      *----------------------------------------------------------------*
       2240-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *     PROCESSAMENTO DA BRAD0660 - INSERCAO DA TELA DCOMIUTM      *
      *----------------------------------------------------------------*
       2300-PROCESSAR-BRAD0660         SECTION.
      *----------------------------------------------------------------*

           MOVE LENGTH                 OF WRK-OUTPUT-IUTM
                                       TO WRK-OUT-LL-IUTM
                                          WRK-660-LL-MENSAG-IUTM.
           MOVE LENGTH                 OF WRK-660-DCOMIUTM
                                       TO WRK-660-LL-AREA-IUTM.

           CALL 'BRAD0660'             USING WRK-OUTPUT-IUTM
                                             WRK-660-DCOMIUTM.

           IF  RETURN-CODE             NOT EQUAL ZEROS
               MOVE 'APL'              TO ERR-TIPO-ACESSO
               MOVE RETURN-CODE        TO WRK-RETURN-CODE
               MOVE 10                 TO WRK-LOCAL-ERRO
               MOVE WRK-ERRO-BRAD0660  TO ERR-TEXTO
               PERFORM 9999-PROCESSAR-ROTINA-ERRO
           END-IF.

           MOVE WRK-ISRT               TO WRK-FUNCAO.
           MOVE WRK-OUTPUT-IUTM        TO WRK-MENSAGEM.

      *----------------------------------------------------------------*
       2300-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *     FINALIZAR O PROGRAMA                                       *
      *----------------------------------------------------------------*
       3000-FINALIZAR                  SECTION.
      *----------------------------------------------------------------*

            GOBACK.

      *----------------------------------------------------------------*
       3000-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *     PROCESSAMENTO DE ERRO                                      *
      *----------------------------------------------------------------*
       9999-PROCESSAR-ROTINA-ERRO      SECTION.
      *----------------------------------------------------------------*

           IF  ERR-MODULO              NOT EQUAL SPACES AND LOW-VALUES
               MOVE 'DCOM3928'         TO ERR-PGM
           ELSE
               MOVE 'SENHAS02'         TO ERR-PGM
               MOVE 'DCOM3928'         TO ERR-MODULO
           END-IF.

           MOVE WRK-COD-USER           TO ERR-COD-USER.
           MOVE WRK-COD-DEPTO          TO ERR-COD-DEPTO.

           IF  ERR-TIPO-ACESSO         EQUAL 'DB2'
               CALL 'BRAD7100'         USING LNK-IO-PCB
                                             ERRO-AREA
                                             LNK-ALT-PCB
                                             WRK-SQLCA
           ELSE
               CALL 'BRAD7100'         USING LNK-IO-PCB
                                             ERRO-AREA
                                             LNK-ALT-PCB
           END-IF.

           PERFORM 3000-FINALIZAR.

      *----------------------------------------------------------------*
       9999-99-FIM.                    EXIT.
      *----------------------------------------------------------------*
