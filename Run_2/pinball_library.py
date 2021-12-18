import numpy as np
import matplotlib.pyplot as plt
from matplotlib import ticker
from matplotlib.patches import Circle
from matplotlib.tri import Triangulation, UniformTriRefiner, CubicTriInterpolator
from mpl_toolkits.axes_grid1 import make_axes_locatable
import os
import os.path
import time
from scipy.interpolate import griddata
from scipy.integrate import trapz

params = {'text.usetex' : True,
          'font.family' : 'lmodern',
          'text.latex.unicode' : True,
          'axes.titlepad': 10}
plt.rcParams['text.latex.preamble']=[r'\usepackage{lmodern}']
plt.rcParams.update(params)

class PinBall:

    """
    This class implements a number of tools to run simulation of the Fluidic
    Pinball using the UNS3 DNS code written by M. Morzynski.
    """

    def __init__(self):
        self.T = 0

    def read_input_file(self):

        fid = open('Control_Input.dat', 'rb')
        fid.seek(4)

        # Current simulation time.
        self.T = np.fromfile(fid, dtype=np.float64, count=1)[0]
        # Simulation time step.
        self.dt = np.fromfile(fid, dtype=np.float64, count=1)[0]
        self.istep = int(round(self.T/self.dt))

        # Miscellaneous.
        self.nde = np.fromfile(fid, dtype=np.int32, count=1)[0]
        self.nel = np.fromfile(fid, dtype=np.int32, count=1)[0]
        self.nelmax = np.fromfile(fid, dtype=np.int32, count=1)[0]
        nelmax = self.nelmax
        self.ndemax = np.fromfile(fid, dtype=np.int32, count=1)[0]
        ndemax = self.ndemax
        self.ix = np.fromfile(fid, dtype=np.int32, count=6*nelmax).reshape((6, -1), order='F')
        self.nopp = np.fromfile(fid, dtype=np.int32, count=ndemax)

        # Mesh.
        self.x = np.fromfile(fid, dtype=np.float64, count=ndemax)
        self.y = np.fromfile(fid, dtype=np.float64, count=ndemax)

        # Volume of each element.
        self.vol_xg = np.fromfile(fid, dtype=np.float64, count=ndemax)
        self.vol_yg = np.fromfile(fid, dtype=np.float64, count=ndemax)

        # Number of volume forces.
        self.nvf = np.fromfile(fid, dtype=np.int32, count=1)[0]

        # Number of Dirichlet boundary conditions.
        self.nbc = np.fromfile(fid, dtype=np.int32, count=1)[0]

        # Node index of the Dirichlet boundary conditions.
        self.kbc = np.fromfile(fid, dtype=np.int32, count=ndemax*4).reshape((ndemax, -1), order='F')

        # Miscellaneous.
        self.vbco = np.fromfile(fid, dtype=np.float64, count=ndemax*3).reshape((ndemax, -1), order='F')

        # Velocity prescribed at the Dirichlet boundary conditions.
        self.vbc = np.fromfile(fid, dtype=np.float64, count=ndemax*3).reshape((ndemax, -1), order='F')

        # Velocity field.
        self.velocity = np.fromfile(fid, dtype=np.float64, count=ndemax*3*5).reshape((ndemax*3, -1), order='F')

        # Pressure field.
        self.pressure = np.fromfile(fid, dtype=np.float64, count=ndemax)

        # Close file.
        fid.close()

        return self

    def compute_vorticity(self):

        # Miscellaneous.
        nel = self.nel
        nde = self.nde
        ix = self.ix

        # Mesh arrays.
        x = self.x[:nde]
        y = self.y[:nde]

        # Velocity arrays.
        u = self.velocity[self.nopp[:nde]-1, 0]
        v = self.velocity[self.nopp[:nde], 0]

        # Construct the connectivity table.
        conn = np.zeros((4*nel, 3)).astype('int32')
        for i in xrange(nel):
            conn[4*(i+1)-3-1, :] = [abs(ix[0, i]), abs(ix[5, i]), abs(ix[3, i])]
            conn[4*(i+1)-2-1, :] = [abs(ix[3, i]), abs(ix[5, i]), abs(ix[4, i])]
            conn[4*(i+1)-1-1, :] = [abs(ix[4, i]), abs(ix[5, i]), abs(ix[2, i])]
            conn[4*(i+1)-0-1, :] = [abs(ix[3, i]), abs(ix[4, i]), abs(ix[1, i])]

        conn -= 1   # Account fot the python 0-based indexing.

        # Compute the vorticity.
        dx, dy = 0, 0
        q2max, q2min = 0, 0
        vort = np.zeros_like(x)
        ind = np.zeros_like(x)

        for kc in xrange(4*nel):
            k1, k2, k3, k4 = conn[kc, 0], conn[kc, 1], conn[kc, 2], conn[kc, 0]
            x1, x2, x3, x4 = x[k1], x[k2], x[k3], x[k4]
            y1, y2, y3, y4 = y[k1], y[k2], y[k3], y[k4]
            u1, u2, u3, u4 = u[k1], u[k2], u[k3], u[k4]
            v1, v2, v3, v4 = v[k1], v[k2], v[k3], v[k4]
            dx = max(dx, max(x1, x2, x3, x4)-min(x1, x2, x3, x4))

            area = ( (x3-x1)*(y4-y2) + (x2-x4)*(y3-y1) )
            circ = ( (u1+u2)*(x2-x1) + (v1+v2)*(y2-y1)
                 + (u2+u3)*(x3-x2) + (v2+v3)*(y3-y2)
                 + (u3+u4)*(x4-x3) + (v3+v4)*(y4-y3)
                 + (u4+u1)*(x1-x4) + (v4+v1)*(y1-y4) ) / area

            vort[k1] = vort[k1] + circ
            ind[k1] += 1
            vort[k2] = vort[k2] + circ
            ind[k2] += 1
            vort[k3] = vort[k3] + circ
            ind[k3] += 1

            if k4 != k1:
                vort[k4] = vort[k4] + circ
                ind[k4] += 1

        for i in xrange(nde):

            vort[i] /= ind[i]

            if q2max < vort[i]:
                q2max = vort[i]

            if q2min > vort[i]:
                q2min = vort[i]

        self.vorticity = vort

        return self

    def apply_control(self, omega):

        # Apply the control law for the first cylinder.
        # Location of its center : (x, y) = (0, 0.75)
        for i in xrange(144):
            # Get the corresponding node index.
            j = self.kbc[i, 0]-1
            # Local coordinates w.r.t. to the cylinder's center.
            xc, yc = 0, 0.75
            xl, yl = self.x[j]-xc, self.y[j]-yc
            # Enfore the control law.
            self.vbc[i, 0] = -omega[0]*0.5*yl
            self.vbc[i, 1] = omega[0]*0.5*xl

        # Apply the control law for the first cylinder.
        # Location of its center : (x, y) = (0, -0.75)
        for i in xrange(234, 378):
            # Get the corresponding node index.
            j = self.kbc[i, 0]-1
            # Local coordinates w.r.t. to the cylinder's center.
            xc, yc = 0, -0.75
            xl, yl = self.x[j]-xc, self.y[j]-yc
            # Enfore the control law.
            self.vbc[i, 0] = -omega[1]*0.5*yl
            self.vbc[i, 1] = omega[1]*0.5*xl

        # Apply the control law for the first cylinder.
        # Location of its center : (x, y) = (-1.5*sqrt(0.75), 0)
        for i in xrange(378, 522):
            # Get the corresponding node index.
            j = self.kbc[i, 0]-1
            # Local coordinates w.r.t. to the cylinder's center.
            xc, yc = -1.5*np.sqrt(0.75), 0
            xl, yl = self.x[j]-xc, self.y[j]-yc
            # Enfore the control law.
            self.vbc[i, 0] = -omega[2]*0.5*yl
            self.vbc[i, 1] = omega[2]*0.5*xl

        # Write the Control_Output.dat file.
        fid = open('Control_Output.dat', 'wb')
        self.vbc.T.tofile(fid)
        self.vol_xg.tofile(fid)
        self.vol_yg.tofile(fid)
        fid.close()

        # Remove the Control_Barrier file.
        import os
        os.system('rm -f Control_Barrier')

        return self

    def visualization(self, savename='figure'):


        # --> Sets figure.
        fig = plt.figure()

        # Mesh arrays.
        x = self.x[:self.nde]
        y = self.y[:self.nde]

        # Vorticity array.
        vorticity = self.vorticity[:self.nde]

        # Delaunay triangulation.
        tri = Triangulation(x, y)

        # Mask unwanted triangles.
        radius = 0.5
        xmid = tri.x[tri.triangles].mean(axis=1)
        ymid = tri.y[tri.triangles].mean(axis=1)

        xc1, yc1 = 0, 0.75
        xc2, yc2 = 0, -0.75
        xc3, yc3 = -1.5*np.sqrt(0.75), 0
        mask = np.where((xmid-xc1)**2 + (ymid-yc1)**2 < radius**2, 1, 0)
        mask += np.where((xmid-xc2)**2 + (ymid-yc2)**2 < radius**2, 1, 0)
        mask += np.where((xmid-xc3)**2 + (ymid-yc3)**2 < radius**2, 1, 0)
        tri.set_mask(mask)

        # Plot the vorticity field.
        ax = fig.gca()
        cmap = plt.cm.RdBu
        h = ax.tripcolor(tri, vorticity, shading='gouraud', cmap=cmap)
        h.set_clim(-4, 4)
        ax.set_aspect('equal')
        ax.set_xlim(-5, 20)
        ax.set_ylim(-4, 4)

        # Set up the colorbar.
        divider = make_axes_locatable(ax)
        cax = divider.append_axes('right', size='5%', pad=0.1)
        cb = plt.colorbar(h, cax=cax)
        tick_locator = ticker.MaxNLocator(nbins=5)
        cb.locator = tick_locator
        cb.update_ticks()

        # Highlight the cylinders.
        xc, yc, r = 0., 0.75, 0.5
        circle = Circle((xc, yc), r, facecolor='None', edgecolor='k')
        ax.add_patch(circle)

        xc, yc, r = 0., -0.75, 0.5
        circle = Circle((xc, yc), r, facecolor='None', edgecolor='k')
        ax.add_patch(circle)

        xc, yc, r = -1.5*np.sqrt(0.75), 0., 0.5
        circle = Circle((xc, yc), r, facecolor='None', edgecolor='k')
        ax.add_patch(circle)

        # Titles, labels, ...
        ax.set_xlabel(r'$x$')
        ax.locator_params(axis='y', nbins=5)
        ax.set_ylabel(r'$y$', rotation=0)

        # Save the figure.
        fig.savefig(savename+'.png', bbox_inches='tight', dpi=300)

        return fig, ax

    def cfd(self):
        tmp = False
        while tmp is False:
            if os.path.exists('Control_Barrier') == True:
                break
        return

    def start_pinball(self):
        os.system('rm -f Control_Barrier')
        os.system('./UNS3 > logfile &')
        return

    def end_pinball(self):
        os.system('killall UNS3')
        return

    def compute_pressure_force(self):
        # --> Compute the pressure coefficient on the top cylinder.
        x = np.zeros((144, ))
        y, p, theta = np.zeros_like(x), np.zeros_like(x), np.zeros_like(x)
        xc, yc = 0, 0.75
        for i in xrange(144):
            j = self.kbc[i, 0]-1 # NOTE: The -1 is for the 0-based python indexing compared to the 1-based in Matlab.
            x[i] = self.x[j]
            y[i] = self.y[j]
            p[i] = self.pressure[j]
            theta[i] = np.arctan2(y[i]-yc, x[i]-xc)

        theta, idx = np.unique(theta, return_index=True)
        p = p[idx]
        lift_1 = -trapz(p*np.sin(theta), theta)
        drag_1 = -trapz(p*np.cos(theta), theta)

        # --> Compute the pressure coefficient on the bottom cylinder.
        p = np.zeros_like(x)
        theta = np.zeros_like(x)
        xc, yc, k = 0, -0.75, 0
        for i in xrange(234, 378):
            j = self.kbc[i, 0]-1
            x[k] = self.x[j]
            y[k] = self.y[j]
            p[k] = self.pressure[j]
            theta[k] = np.arctan2(y[k]-yc, x[k]-xc)
            k += 1

        theta, idx = np.unique(theta, return_index=True)
        p = p[idx]
        lift_2 = -trapz(p*np.sin(theta), theta)
        drag_2 = -trapz(p*np.cos(theta), theta)

        # --> Compute the pressure coefficient on the front cylinder.
        p = np.zeros_like(x)
        theta = np.zeros_like(x)
        xc, yc, k = -1.5*np.sqrt(0.75), 0, 0
        for i in xrange(378, 522):
            j = self.kbc[i, 0]-1
            x[k] = self.x[j]
            y[k] = self.y[j]
            p[k] = self.pressure[j]
            theta[k] = np.arctan2(y[k]-yc, x[k]-xc)
            k += 1

        theta, idx = np.unique(theta, return_index=True)
        p = p[idx]
        lift_3 = -trapz(p*np.sin(theta), theta)
        drag_3 = -trapz(p*np.cos(theta), theta)

        self.lift = np.array([lift_1, lift_2, lift_3])
        self.drag = np.array([drag_1, drag_2, drag_3])

        return self

    def sensors_measurements(self):
        # --> Define the array for the sensors measurements.
        s = np.array((3, ))
        # --> Define the rack of sensors.
        xs = np.ones((5,))*5.5
        ys = np.zeros_like(xs)
        for i in xrange(ys.size):
            ys[i] = 0.75*(2-i)

        # --> Get the sensors measurements from the data.
        x, y = self.x[:self.nde], self.y[:self.nde]
        u = self.velocity[self.nopp[:self.nde]-1, 0]
        v = self.velocity[self.nopp[:self.nde], 0]
        su = griddata((x, y), u, (xs, ys))
        sv = griddata((x, y), v, (xs, ys))
        self.sensors = np.asarray([su, sv])

        return self

if __name__ == '__main__':

    # --> Set up the control law.
    def compute_control_law(pinball):
        omega = np.zeros((3,))
        return omega

    # Instantiate the pinball class.
    pinball = PinBall()

    # Start the Pinball simulation.
    pinball.start_pinball()

    snapshots, lift, drag, control, sensors = [], [], [], [], []

    i, j = 0, 0

    # Begining of the time loop.
    while pinball.T < 200:

        # Wait until the CFD solver has finished its current calculation.
        pinball.cfd()

        # --> Read-in the fortran-written file for UNS3 solver.
        pinball.read_input_file()
        if np.mod(pinball.istep, 10) == 0:
            print 'Simulation time : %.2f' %pinball.T

        # --> Compute the vorticity.
        pinball.compute_vorticity()

        # --> Visualization.
        if np.mod(pinball.istep, 10) == 0:
            fig, _ = pinball.visualization(savename='figure_%i' %i)
            i += 1
            plt.close()

            # --> Store snapshot.
            snapshots.append(pinball.vorticity[:pinball.nde])

        # --> Compute the drag and lift.
        pinball.compute_pressure_force()
        pinball.sensors_measurements()

        lift.append(pinball.lift)
        drag.append(pinball.drag)
        sensors.append(pinball.sensors)

        # --> Compute the control law.
        omega = compute_control_law(pinball)
        control.append(omega)
        if np.mod(pinball.istep, 10) == 0:
            print 'Angular velocities :', omega
            print 'Lift force         :', pinball.lift
            print 'Drag force         :', pinball.drag
            print '---------------------------------------- \n'

        # --> Apply the control law.
        pinball.apply_control(omega)

    pinball.end_pinball()

    # --> Final output.
    snapshots = np.asarray(snapshots).T
    lift = np.asarray(lift)
    drag = np.asarray(drag)
    control = np.asarray(control)

    import pickle
    output = {'lift': lift,
              'drag': drag,
              'control': control,
              'pinball': pinball,
              'snapshots': snapshots,
              'sensors': sensors
              }

    pickle.dump(output, open('dataset_4.p', 'wb'))
