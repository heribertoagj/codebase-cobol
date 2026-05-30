      *----------------------------------------------------------------*
      * BVVEW481-  AREA DE DADOS DB2                                   *
      *            ACESSO DB2 A TABELA DE LOG EVENTO                   *
      *----------------------------------------------------------------*
       01  LK-7481-AREA-LOGEVENTO.
           05 LK-7481-CD-RETORNO            PIC S9(004) COMP.
           05 LK-7481-DS-RETORNO            PIC  X(080).
           05 LK-7481-CD-PROGRAMA           PIC  X(008).
           05 LK-7481-TP-ACESSO             PIC  X(003).
           05 LK-7481-NR-ACESSO             PIC  9(002).
           05 LK-7481-AREA-INPUT.
              10 LK-7481-HLOG-EVNTO-INI     PIC  X(026).
              10 LK-7481-HLOG-EVNTO-FIM     PIC  X(026).
              10 LK-7481-CLOG-EVNTO-IN      PIC S9(010)V USAGE COMP-3.
              10 LK-7481-CTPO-EVNTO-IN      PIC  X(003).
           05 LK-7481-AREA-DB2.
              10 LK-7481-HLOG-EVNTO         PIC  X(026).
              10 LK-7481-CLOG-EVNTO         PIC S9(010)V USAGE COMP-3.
              10 LK-7481-CTPO-EVNTO         PIC  X(003).
              10 LK-7481-RCONTD-EVNTO.
                 15 LK-7481-RCONTD-EVNTO-LEN
                                            PIC S9(004) USAGE COMP.
                 15 LK-7481-RCONTD-EVNTO-TEXT
                                            PIC X(3500).
