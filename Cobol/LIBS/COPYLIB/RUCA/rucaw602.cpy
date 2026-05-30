      ******************************************************************
      * NOME BOOK : RUCAW602                                           *
      * DESCRICAO : AREA PARA CHAMADA DO RUCA7602                      *
      * DATA      : 02/2026                                            *
      * AUTOR     : HERIBERTO GIANNASI                                 *
      * COMPONENTE: SISTEMA RUCA                                       *
      * CONTAHO   : 82 BYTES                                           *
      ******************************************************************
      ******************************************************************
      * DATA       AUTOR             DESCRICAO / MANUTENCAO            *
      ******************************************************************
      * XX/XX/XXXX XXXXXXXXXXXXXXXXX XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX  *
      ******************************************************************
          03 RUCAW602-ENTRADA.
             05 RUCAW602-E-CCONTR            PIC 9(09).
*************** VARIAVEL PARA PERMITIR TESTES COM DEBUG
             05 RUCAW602-E-TPO-ACESSO        PIC X(01).
*************** B=BATCH / OUTROS=IMS ONLINE
          03 RUCAW602-SAIDA.
             05 RUCAW602-S-SLD-DEVEDOR       PIC 9(13)V99 COMP-3.
             05 RUCAW602-S-SLD-ENCARGOS      PIC 9(13)V99 COMP-3.
             05 RUCAW602-S-SLD-TOTAL         PIC 9(13)V99 COMP-3.
             05 RUCAW602-S-NUM-PARCELAS      PIC 9(03)    COMP-3.
             05 RUCAW602-S-PARCELAS          OCCURS 120.
                10 RUCAW602-S-PCELA-NUM      PIC 9(03).
                10 RUCAW602-S-PCELA-DTEVENTO PIC X(10).
                10 RUCAW602-S-PCELA-DTVCTO   PIC X(10).
                10 RUCAW602-S-PCELA-VLRPRIN  PIC 9(13)V99 COMP-3.
                10 RUCAW602-S-PCELA-VLRENC   PIC 9(13)V99 COMP-3.
                10 RUCAW602-S-PCELA-CODLANC  PIC 9(04).
          03 RUCAW602-RETORNO.
             05 RUCAW602-RET-CODE            PIC 9(02).
             05 RUCAW602-RET-MENSAGEM        PIC X(79).
