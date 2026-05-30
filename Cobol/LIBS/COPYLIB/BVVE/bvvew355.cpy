      *===============================================================*
      * IDENTIFICACAO...........: BVVEW355                            *
      * DATA DE CRIACAO ........: SET/2020                            *
      * ANALISTA RESPONSAVEL ...: FABRICIO MASAKIYO MANCINI           *
      * --------------------------------------------------------------*
      * DESCRICAO:       AREA DE COMUNICAO ENTRE PROGRAMAS            *
      *                                                               *
      * PROGRAMA-CHAMADOR - NOME DO PROGRAMA CHAMADOR                 *
      * DINCL-RESUL-MNTRC - DATA DO MONITORAMENTO                     *
      * CSNLZD-MSGRA      - CODIGO STATUS                             *
      * RSNLZD-MSGRA      - DESCRICAO STATUS                          *
      * CFASE-PROCM       - CODIGO FASE PROCESSAMENTO DO ERRO         *
      * RFASE-PROCM       - DESCRICAO FASE PROCESSAMENTO DO ERRO      *
      * CSIST-EXTER       - SISTEMA EXTERNO                           *
      * CEVNTO-MSGEM      - EVENTO MENSAGEM                           *
      * CFLUXO-MSGEM      - FLUXO DA MENSAGEM                         *
      * CISPB-EMISR-MSGEM - IDENTIFICADOR DA INSTITUICAO EMISSORA     *
      * HINTERVALO        - INTERVALO PESQUISA MENSAGEM               *
      * CUNIC-TRFGO-MSGEM - CODIGO UNICO DE TRAFEGO DE MENSAGEM       *
      * TIMESTAMP          - HORA DO MONITORAMENTO                    *
      * HINTERVALO-INI-ABS - INICIO INTERVALO - HORA ABSOLUTA         *
      * HINTERVALO-FIM-ABS - FIM INTERVALO    - HORA ABSOLUTA         *
      * NMNTRC-TRFGO-ARQ   - NUMERO DE MONITORACAO DE TRAFEGO ARQUIVO *
      * CFUNC-BDSCO        - FUNCIONARIO BRADESCO                     *
      * ENVIADA-RECEBIDA   - MENSAGEM ENVIADA OU RECEBIDA             *
      *===============================================================*
      * ALTERACOES:                                                   *
      *                                                               *
      * DATA        NOME       DESCRICAO                              *
      * =========== ========== ====================================== *
FEV18 * 15/02/2018  PRIME      INCLUSAO COD-DEPTO / HORA-CORTE        *
      *===============================================================*
           05  BVVE355-AREA-COMUNICACAO.
               10 BVVE355-PROGRAMA-CHAMADOR    PIC X(008).
               10 BVVE355-DINCL-RESUL-MNTRC    PIC X(010).
               10 BVVE355-CSNLZD-MSGRA         PIC 9(003).
               10 BVVE355-RSNLZD-MSGRA         PIC X(100).
               10 BVVE355-CFASE-PROCM          PIC 9(004).
               10 BVVE355-RFASE-PROCM          PIC X(100).
               10 BVVE355-CEVNTO-MSGEM         PIC X(004).
               10 BVVE355-CFLUXO-MSGEM         PIC X(003).
               10 BVVE355-CISPB-EMISR-MSGEM    PIC X(008).
               10 BVVE355-HINTERVALO-INI       PIC X(005).
               10 BVVE355-HINTERVALO-FIM       PIC X(005).
               10 BVVE355-CUNIC-TRFGO-MSGEM    PIC X(023).
               10 BVVE355-PERIODO              PIC 9(004).
               10 BVVE355-TIMESTAMP            PIC X(026).
               10 BVVE355-HINTERVALO-INI-ABS   PIC S9(015)  COMP-3.
               10 BVVE355-HINTERVALO-FIM-ABS   PIC S9(015)  COMP-3.
               10 BVVE355-NMNTRC-TRFGO-ARQ     PIC 9(018).
               10 BVVE355-CFUNC-BDSCO          PIC X(007).
               10 BVVE355-ENVIADA-RECEBIDA     PIC X(001).
FEV18          10 BVVE355-AGCIA-DEPTO-REC      PIC 9(004).
FEV18          10 BVVE355-HORA-CORTE           PIC S9(015)  COMP-3.
