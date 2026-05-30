      *---------------------------------------------------------------*
      *   NOME INC             -DDNAME-                LENGTH  0150   *
      *   I#DCOMBT              ------                 SISTEMA DCOM   *
      *---------------------------------------------------------------*
      *                                                               *
      *   DESCRICAO: LAYOUT DE ARQUIVO DE LIQUIDACAO DCOM ENVIADO     *
      *   AO BVVE.                                                    *
      *                                                               *
      *---------------------------------------------------------------*
       01  DCOMBT-REG.
           05 BT-DANO-OPER-DESC              PIC S9(04)        COMP-3.
           05 BT-NSEQ-OPER-DESC              PIC S9(09)        COMP-3.
           05 BT-NPCELA-DESC-COML            PIC S9(05)        COMP-3.
           05 BT-CBCO                        PIC S9(03)        COMP-3.
           05 BT-CAG-BCRIA                   PIC S9(05)        COMP-3.
           05 BT-CCTA-BCRIA-CLI              PIC S9(13)        COMP-3.
           05 BT-CCNPJ-CPF                   PIC S9(09)        COMP-3.
           05 BT-CFLIAL-CNPJ                 PIC S9(05)        COMP-3.
           05 BT-CCTRL-CNPJ-CPF              PIC S9(02)        COMP-3.
           05 BT-CADM-CATAO-CREDT            PIC S9(09)        COMP-3.
      *--- CODIGO DO PRODUTO BVVE
           05 BT-CINDCD-VDA-RCBVL            PIC  X(01).
      *--- ROTATIVO OU PARCELADO
           05 BT-CCRDRA-ESTBL-COML           PIC  9(04).
      *--- CODIGO DA CREDENCIADORA - DOMINIO BVVE
           05 BT-CBANDE-VISAO                PIC  9(03).
      *--- CODIGO DA BANDEIRA - DOMINIO BVVE
           05 BT-DATA-VENCIMENTO             PIC  X(10).
           05 BT-VPRINC-PCELA-DESC           PIC S9(15)V99     COMP-3.
           05 BT-VALOR-CRED-RESERVADO        PIC S9(15)V99     COMP-3.
      *--- VALOR RESERVADO NO BVVE
           05 BT-RESERVA                     PIC  X(76).
