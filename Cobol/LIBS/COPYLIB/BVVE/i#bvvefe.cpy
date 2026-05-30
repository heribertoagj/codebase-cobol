      *----------------------------------------------------------------*
      *  I#BVVEFE - NOVOS NORMATIVOS (RESOLUCAO 4734)                  *
      *  ARQUIVO DE PARTICIPANTES EXTRAIDO
      *  TAMANHO: 161                                                  *
      *----------------------------------------------------------------*

       01  BVVEFE-REGISTRO.
           03 BVVEFE-GRUPO-PARTIC.
              05 BVVEFE-CHV-GRUPO                 PIC X(008).
              05 FILLER                           PIC X(008).
              05 BVVEFE-TP-PSSOA                  PIC X(008).
              05 FILLER                           PIC X(008).
              05 BVVEFE-CNPJ-CPF-PART             PIC X(014).
              05 FILLER                           PIC X(008).
              05 BVVEFE-NOME-PARTIC               PIC X(050).
              05 FILLER                           PIC X(008).
              05 BVVEFE-NUM-TEL-PART              PIC X(011).
              05 FILLER                           PIC X(008).
              05 BVVEFE-EMAIL-PART                PIC X(050).
              05 FILLER                           PIC X(008).
              05 BVVEFE-TP-PART                   PIC X(002).
              05 FILLER                           PIC X(008).
              05 BVVEFE-DT-ADES-HOMOL             PIC X(008).
              05 FILLER                           PIC X(008).
              05 BVVEFE-DT-ADES-PROD              PIC X(008).
              05 FILLER                           PIC X(008).
              05 BVVEFE-INDR-DOMCL                PIC X(001).
      *
      *  BVVEFE-TP-PSSOA: 'PF' OU 'PJ'
      *  BVVEFE-TP-PART :
      *  01 INSTITUIÁ„O FINANCEIRA
      *  02 INSTITUIÁ„O N„O FINANCEIRA
      *  03 CREDENCIADORA
      *  04 SUBCREDENCIADORA
