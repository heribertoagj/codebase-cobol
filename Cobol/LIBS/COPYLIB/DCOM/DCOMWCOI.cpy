      *================================================================*
      * NOME BOOK : DCOMWCOI - INTERFACE DE DADOS DE ENTRADA           *
      * DESCRICAO : OBTER TAXA, SPREAD E CUSTO NO SISTEMA COTX         *
      * DATA      : 09/04/2024                                         *
      * GRUPO     : CPM BRAXIS                                         *
      * COMPONENTE: FUNCIONAL                                          *
      * TAMANHO   : 30000 BYTES                                        *
      *================================================================*
           05 DCOMWCOI-HEADER.
             10 DCOMWCOI-COD-LAYOUT    PIC X(008)  VALUE 'DCOMWCOI'.
             10 DCOMWCOI-TAM-LAYOUT    PIC 9(005)  VALUE 30000.

           05 DCOMWCOI-REGISTRO.
      *--> DADOS DE ENTRADA - GERAL
             10 DCOMWCOI-BLOCO-ENTRADA.
                15 DCOMWCOI-CCLUB                  PIC  9(010).
                15 DCOMWCOI-CPRODT-SERVC-OPER      PIC  9(008).
                15 DCOMWCOI-CPRODT                 PIC  9(003).
                15 DCOMWCOI-CSPROD-DESC-COML       PIC  9(003).
                15 DCOMWCOI-CINDCD-CALC-JURO       PIC  X(001).
                15 DCOMWCOI-CBASE-TAXA             PIC  9(03).
                15 DCOMWCOI-CTPO-CAPTZ             PIC  9(02).
                15 DCOMWCOI-CTPO-NEG-IOF           PIC  X(01).
                15 DCOMWCOI-CTPO-NEG-TARIFA        PIC  X(01).
                15 DCOMWCOI-VSPREAD                PIC S9(007)V9(005).
                15 DCOMWCOI-VTAXA                  PIC S9(007)V9(005).
                15 DCOMWCOI-VTARIF                 PIC S9(016)V9(002).
                15 DCOMWCOI-ALIQ-IOF-NORMAL        PIC S9(003)V9(009).
                15 DCOMWCOI-ALIQ-IOF-ADICIONAL     PIC S9(003)V9(009).
                15 DCOMWCOI-APLIC-OBRIGATORIA      PIC  X(001).
                15 DCOMWCOI-DINIC-OPER             PIC  9(008).
                15 DCOMWCOI-TIPO-PESSOA            PIC  X(001).
                15 DCOMWCOI-CANAL                  PIC  9(003).
                15 DCOMWCOI-NPCELA                 PIC  9(004).
                15 DCOMWCOI-DADOS-PARCELA   OCCURS 405 TIMES.
                   20 DCOMWCOI-DTVCTO              PIC  9(008).
                   20 DCOMWCOI-QDIA-CORRI          PIC  9(005).
                   20 DCOMWCOI-QDIA-UTIL           PIC  9(005).
                   20 DCOMWCOI-CIND-JURO-PCELA     PIC  X(001).
                   20 DCOMWCOI-CIND-PRINC-PCELA    PIC  X(001).
                   20 DCOMWCOI-VPCELA-FEE          PIC S9(015)V9(002).
                   20 DCOMWCOI-VFUTUR              PIC S9(015)V9(002).
                   20 DCOMWCOI-VPRESENTE           PIC S9(015)V9(002).
             10 FILLER                             PIC X(0545).

      *--> DADOS DE SAIDA                                              *
             10 DCOMWCOI-BLOCO-SAIDA.
                15 DCOMWCOI-TAXA-360-AA            PIC S9(007)V9(005).
                15 DCOMWCOI-TAXA-360-AM            PIC S9(007)V9(005).
                15 DCOMWCOI-TAXA-CUSTO-360-AA      PIC S9(007)V9(005).
                15 DCOMWCOI-TAXA-CUSTO-360-AM      PIC S9(007)V9(005).
                15 DCOMWCOI-PSPREAD-PRECO-AA       PIC S9(007)V9(005).
                15 DCOMWCOI-PSPREAD-PRECO-AM       PIC S9(007)V9(005).
             10 FILLER                             PIC  X(500).
