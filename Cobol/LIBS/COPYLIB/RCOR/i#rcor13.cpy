      *===============================================================*
      * INC          : I#RCOR13                                       *
      * DESCRICAO    : AREA DE COMUNICACAO PROGRAMA RCOR6010 - INCLUI *
      *                DADOS DAS MENSAGENS NAS BASES DO RCOR          *
      * DATA CRIACAO : AGO/2012                                       *
      * AUTOR        : PRIME INFORMATICA                              *
PR0013* TAMANHO      : 30538                                          *
      * ------------------------------------------------------------- *
      * ALTERACOES:                                                   *
      *    DATA           NOME                   DESCRICAO            *
      * ==========  ================ ================================ *
PR0013* 31/10/2016  ELIEZER SIQUEIRA NOVOS CAMPOS                     *
-     *                              CTPO-ERRO-MSGEM...: TIPO ERRO    *
-     *                              RERRO-MSGEM-CREDT.: DESC. ERRO   *
PR0013*                              TPO E DESC. QUANDO ERRO NO LEGADO*
      * ------------------------------------------------------------- *
      * CAMPO:                                                        *
      * RCOR13-TP-INCL = 1 -> INCL RCORB000/RCORB003 (COR0001/COR0002)*
      * RCOR13-TP-INCL = 2 -> INCL RCORB003          (MSGENS RESPOSTA)*
      * RCOR13-TP-INCL = 3 -> ATUALIZA MQID RCORB000                  *
      * RCOR13-TP-INCL = 5 -> TRATAMENTO PARA A GEN0004               *
      * RCOR13-TP-INCL = 6 -> TRATAMENTO PARA MENSAGENS ENVIADAS PELA *
      *                       ENTRADA MANUAL - TMNF                   *
      * RCOR13-TP-INCL = 7 -> ATUALIZA INDICADOR ENVIO MSG LEGADO     *
      * RCOR13-TP-INCL = 8 -> ATUALIZA INDICADOR ENVIO MSG LEGADO     *
PR0013* RCOR13-TP-INCL = 9 -> ERRO NA NOTIFICACAO DO LEGADO           *
PR0013*                       ATU.: INDICADOR, MOTIVO E TPO ERRO      *
      *===============================================================*
       01 RCOR13-REGISTRO.
          03 RCOR13-CONTROLE.
             05 RCOR13-TP-INCL           PIC  9(001)      VALUE ZEROS.
             05 RCOR13-CNRO-CTRL-INSTC   PIC  X(020)      VALUE SPACES.
             05 RCOR13-E-CMSGEM-CREDT    PIC  X(009)      VALUE SPACES.
             05 RCOR13-E-CCTRL-MSGEM     PIC  9(013)      VALUE ZEROS.
             05 RCOR13-E-HINCL-REG       PIC  X(026)      VALUE SPACES.
             05 RCOR13-SEQ-MSG           PIC  9(003)      VALUE ZEROS.
          03 RCOR13-AREA-RCORB000.
             05 RCOR13-CENTRO-CUSTO      PIC  X(004)      VALUE SPACES.
             05 RCOR13-CSIT-PROC-CREDT   PIC  9(001)      VALUE ZEROS.
             05 RCOR13-CIDTFD-EMISR      PIC  X(008)      VALUE SPACES.
             05 RCOR13-CIDTFD-DSTNA      PIC  X(008)      VALUE SPACES.
             05 RCOR13-COPER-UNIC-MSGEM  PIC  X(023)      VALUE SPACES.
             05 RCOR13-HGRAV-PROT-CHEGD  PIC  X(026)      VALUE SPACES.
             05 RCOR13-HGRAV-PROT-ENTRG  PIC  X(026)      VALUE SPACES.
             05 RCOR13-CAUT-MSG-CREDT    PIC  X(048)      VALUE SPACES.
          03 RCOR13-AREA-RCORB003.
             05 RCOR13-COD-MSGEM         PIC  X(009)      VALUE SPACES.
             05 RCOR13-MSGEM-CREDT-TXT   PIC  X(30000)    VALUE SPACES.
          03 RCOR13-SAIDA.
             05 RCOR13-S-CMSGEM-CREDT    PIC  X(009)      VALUE SPACES.
             05 RCOR13-S-CCTRL-MSGEM     PIC  9(013)      VALUE ZEROS.
             05 RCOR13-S-HINCL-REG       PIC  X(026)      VALUE SPACES.
             05 RCOR13-S-COD-RETORNO     PIC  9(005)      VALUE ZEROS.
             05 RCOR13-S-DSC-ERRO        PIC  X(079)      VALUE SPACES.
PR0013    03 RCOR13-DADOS-COMPL.
-            05 RCOR13-CTPO-ERRO-MSGEM   PIC  9(001)      VALUE ZEROS.
-            05 RCOR13-RERRO-MSGEM-CREDT PIC  X(080)      VALUE SPACES.
-         03 RCOR13-RESERVA.
PR0013       05 RCOR13-FILLER            PIC  X(100)      VALUE SPACES.
