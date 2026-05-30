      *
      *****************************************************************
      * NOME DA INC - GFCTWBC0                                        *
      * DESCRICAO   - TARIFAS - CARGA MASSIVA DE CESTAS               *
      *               CADMASS  - NAO-CELULAR                          *
      *               UADMASSC - CELULAR                              *
      * TAMANHO     - 178                                             *
      * DATA        - MAR/2013                                        *
      * RESPONSAVEL - PAGNOCCA - SONDA IT                             *
      *****************************************************************
      *
       01  GFCTWBC0-REG.
           05 GFCTWBC0-RG.
              10  BC0-AGENCIA           PIC  X(005).
              10  BC0-CONTA             PIC  X(007).
              10  BC0-CESTA             PIC  X(005).
              10  BC0-OPERADORA         PIC  X(009).
              10  BC0-DDD               PIC  X(003).
              10  BC0-NUM-CEL           PIC  X(009).
              10  BC0-DT-INIC           PIC  X(008).
              10  BC0-DT-FIM            PIC  X(008).
              10  BC0-CENTRO-CUSTO      PIC  X(004).
              10  BC0-MENSAGEM          PIC  X(080).
              10  BC0-RESERVA           PIC  X(040).
           05 GFCTWBC0-RG-R REDEFINES GFCTWBC0-RG.
              10  BC0-AGENCIA-R         PIC  9(005).
              10  BC0-CONTA-R           PIC  9(007).
              10  BC0-CESTA-R           PIC  9(005).
              10  BC0-OPERADORA-R       PIC  9(009).
              10  BC0-DDD-R             PIC  9(003).
              10  BC0-NUM-CEL-R         PIC  9(009).
              10  BC0-DT-INIC-R.
                  15  BC0-DT-INIC-R-D   PIC  9(002).
                  15  BC0-DT-INIC-R-M   PIC  9(002).
                  15  BC0-DT-INIC-R-A   PIC  9(004).
              10  BC0-DT-FIM-R.
                  15  BC0-DT-FIM-R-D    PIC  9(002).
                  15  BC0-DT-FIM-R-M    PIC  9(002).
                  15  BC0-DT-FIM-R-A    PIC  9(004).
              10  BC0-CENTRO-CUSTO-R    PIC  X(004).
              10  BC0-MENSAGEM-R        PIC  X(080).
              10  BC0-RESERVA-R.
                  15  BC0-TMS-R         PIC  X(026).
                  15  FILLER            PIC  X(014).
      *
      *****************************************************************
      * FIM DA INC - GFCTWBC0                                         *
      *****************************************************************
