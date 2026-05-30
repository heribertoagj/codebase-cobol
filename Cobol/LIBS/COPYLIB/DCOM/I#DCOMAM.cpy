      *===============================================================*
      *---------------------------------------------------------------*
      * MODULOS DE ACESSO..:                                          *
      *    - DCOM5900 - TRATAR COR0008R1                              *
      *===============================================================*
       01  AM-ARGUMENTOS-ENTRADA.
       05  AM-E-CCNPJ-ENTID-DESC       PIC S9(09) COMP-3.
       05  AM-E-CFLIAL-CNPJ-DESC       PIC S9(04) COMP-3.
       05  AM-E-CCTRL-CNPJ-DESC        PIC S9(02) COMP-3.
       05  AM-E-DANO-INIC-DESC         PIC S9(06) COMP-3.
       05  AM-E-DANO-FIM-DESC          PIC S9(06) COMP-3.
       05  AM-E-NUM-REF-OCOR           PIC  X(11).
       05  AM-E-NUM-PAG-ATUAL          PIC  9(03) COMP-3.
       05  AM-E-MSGEM-DESC.
       10  AM-E-MSGEM-DESC-LEN         PIC S9(02) COMP-5.
       10  AM-E-MSGEM-DESC-TEXT        PIC  X(4000).

      *****************************************************************
      * AREA DE RETORNO                                               *
      *****************************************************************
       01  AM-RETORNO.
       05  AM-S-COD-RETORNO            PIC  9(004).
       05  AM-S-MSG-RETORNO            PIC  X(079).
